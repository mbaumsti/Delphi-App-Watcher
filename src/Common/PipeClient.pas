(*******************************************************************************
  Project  : AppWatcher
  Unit     : PipeClient
  Author   : Russell (original), Tobias Giesen (packaged), mbaumsti (modularized)
  Original GitHub  : https://github.com/superflexible/NamedPipesForDelphi
  GitHub   : https://github.com/mbaumsti/Delphi-App-Watcher.git
  Date     : 09/04/2025
  Version  : 3.0.0
  License  : The Unlicense (public domain)

  Description :
  -------------
  This unit is part of a named pipes communication framework originally developed
  by Russell and later released as open-source by Tobias Giesen. It provides
  a robust foundation for inter-process communication via Windows Named Pipes,
  supporting both local and networked scenarios (including services).

  The source has been modularized into three parts:
  - PipesCommon: shared constants, types, and core utilities
  - PipeClient: client-side implementation and connection logic
  - PipeServer: server-side implementation and multi-client management

  Features :
  -----------
  - High-performance asynchronous I/O using overlapped structures
  - Client and server components with multi-threaded message handling
  - Thread-safe message queues with support for event callbacks
  - Fully compatible with 32-bit and 64-bit Delphi (tested with Delphi 12)
  - Minimal changes from the original logic, aside from modularization

  Change Log :
  ------------
  - [09/04/2025] : Modularization of the original source into three separate units

  Notes :
  -------
  Original repository: https://github.com/superflexible/NamedPipesForDelphi
  Original authorship by Russell, with packaging and enhancements by Tobias Giesen.
  Modular separation by mbaumsti for integration into the AppWatcher project.
*******************************************************************************)


Unit PipeClient;

Interface

Uses
    PipesCommon,
    Forms, Windows, SysUtils, Classes, Messages, SyncObjs;

Type
    // Forward declarations
    TPipeClient = Class;

    // Pipe Client worker thread class
    TPipeClientThread = Class(TPipeThread)
    private
        // Private declarations
        FNotify: HWND;
        FPipe: HPIPE;
        FErrorCode: Integer;
        FPipeClient: TPipeClient;
        FWrite: DWORD;
        FPipeWrite: PPipeWrite;
        FRcvRead: DWORD;
        FPendingRead: Boolean;
        FPendingWrite: Boolean;
        FRcvStream: TMemoryStream;
        FRcvBuffer: PChar;
        FRcvSize: DWORD;
        FEvents: Array[0..3] Of THandle;
        FOlapRead: TOverlapped;
        FCanTerminateAndFree: Boolean;
        FOlapWrite: TOverlapped;
    protected
        // Protected declarations
        Function QueuedRead: Boolean;
        Function CompleteRead: Boolean;
        Function QueuedWrite: Boolean;
        Function CompleteWrite: Boolean;
        Procedure DoMessage;
        Procedure DoDequeue;
        Procedure Execute; override;
    public
        // Public declarations
        Constructor Create(PipeClient: TPipeClient; Pipe: HPIPE; KillEvent, DataEvent: THandle);
        Destructor Destroy; override;
        Property CanTerminateAndFree: Boolean Read FCanTerminateAndFree Write FCanTerminateAndFree;
    End;

    // Pipe Client component
    TPipeClient = Class(TObject)
    private
        // Private declarations
        FHwnd: HWND;
        FPipe: HPIPE;
        FPipeName: String;
        FServerName: String;
        FConnected: Boolean;
        FWriteQueue: TWriteQueue;
        FWorker: TPipeClientThread;
        FKillEv: THandle;
        FSA: TSecurityAttributes;
        FOPE: TOnPipeError;
        FOPD: TOnPipeDisconnect;
        FOPM: TOnPipeMessage;
        FOPS: TOnPipeSent;
        FCritical: TRTLCriticalSection;
        ReturnValue: Integer;
        FDestroying: Boolean;
        InConnect: Boolean;
    protected
        // Protected declarations
        Procedure SetPipeName(Const Value: String);
        Procedure SetServerName(Const Value: String);
        Function Dequeue: PPipeWrite;
        Procedure RemoveWorkerThread(Sender: TObject);
        Procedure WndMethod(Var Message: TMessage);
    public
        // Public declarations
        FullPipeName: String;
        Constructor Create;
        Destructor Destroy; override;
        Function Connect(Const wait_ms: Integer): Boolean;
        Procedure Disconnect(Const CanWait: Boolean);
        Procedure WaitUntilWriteQueueEmpty(Const MaxWaitSecs: Integer);
        Function Write(Const Buffer; Count: Integer): Boolean;
        Function Busy: Boolean;
        Property Connected: Boolean Read FConnected;
        Property WindowHandle: HWND Read FHwnd;
        Property Pipe: HPIPE Read FPipe;
        Property PipeName: String Read FPipeName Write SetPipeName;
        Property ServerName: String Read FServerName Write SetServerName;
        Property OnPipeDisconnect: TOnPipeDisconnect Read FOPD Write FOPD;
        Property OnPipeMessage: TOnPipeMessage Read FOPM Write FOPM;
        Property OnPipeSent: TOnPipeSent Read FOPS Write FOPS;
        Property OnPipeError: TOnPipeError Read FOPE Write FOPE;
    End;

Implementation

////////////////////////////////////////////////////////////////////////////////
//
//   TPipeClient
//
////////////////////////////////////////////////////////////////////////////////
Constructor TPipeClient.Create;
Begin
    // Perform inherited
    Inherited Create;

    If Not isMainThread Then
        Raise Exception.Create('Must be in main thread to create a TPipeClient.');

    // Set properties
    InitializeSecurity(FSA);
    FKillEv := CreateEvent(@FSA, True, False, Nil);
    FPipe := INVALID_HANDLE_VALUE;
    FConnected := False;
    FWriteQueue := TWriteQueue.Create;
    FWorker := Nil;
    FPipeName := 'PipeServer';
    FServerName := '';
    FHwnd := AllocateHWnd(WndMethod);

    InitializeCriticalSection(FCritical);
    AddValidObject(self);
End;

Destructor TPipeClient.Destroy;
Begin
    If Not isMainThread Then
        Raise Exception.Create('Must be in main thread to destroy a TPipeClient (Pipe:' + FPipeName + ', Server:' + FServerName + ').');

    If Not Assigned(self) Or Not IsValidObject(self) Then
        Exit;

    FDestroying := true;

    RemoveValidObject(self);

    If Assigned(FWorker) And IsValidObject(FWorker) Then Begin // TG 2013 !!!
        FWorker.OnTerminate := Nil;
        FWorker.FPipeClient := Nil;
    End;

    // Disconnect if connected
    Disconnect(false);

    // Free resources
    FinalizeSecurity(FSA);
    CloseHandle(FKillEv);
    FWriteQueue.Free;
    If FHwnd <> 0 Then
        DeAllocateHWnd(FHwnd);
    FHwnd := 0;
    DeleteCriticalSection(FCritical);

    // Perform inherited
    Inherited Destroy;
End;

Function TPipeClient.Connect(Const wait_ms: Integer): Boolean;
Var
    szname: String;
    lpname: Array[0..1023] Of Char;
    resolved: Boolean;
    dwmode: DWORD;
    dwSize: DWORD;
Begin
    If InConnect Then Begin
        Result := FConnected;
        Exit;
    End;
    InConnect := true;
    Try
        // Optimistic result
        result := True;

        // Exit if already connected
        If FConnected Then exit;

{$IFDEF EXTENDEDLOGGING}
        If Assigned(PipesLogProc) Then PipesLogProc('PipeClient.Connect: ' + FPipeName);
{$ENDIF}
        // Set server name resolution
        resolved := False;

        // Check name against local computer name first
        dwSize := SizeOf(lpname);
        If GetComputerName(lpname, dwSize) Then Begin
            // Compare names
            If (CompareText(lpname, FServerName) = 0) Then Begin
                // Server name represents local computer, switch to the
                // preferred use of "." for the name
                szname := '.';
                resolved := True;
            End;
        End;

        // Resolve the server name
        If Not (resolved) Then Begin
            // Blank name also indicates local computer
            If (FServerName = '') Then
                szname := '.'
            Else
                szname := FServerName;
        End;

        // Build the full pipe name
        FullPipeName := Format('\\%s\pipe\%s', [szname, String(FPipeName)]);
        StrCopy(lpname, PChar(String(FullPipeName)));

        // Attempt to wait for the pipe first
        If WaitNamedPipe(PChar(@lpname), wait_ms) Then Begin
            // Attempt to create client side handle
            FPipe := CreateFile(PChar(@lpname), GENERIC_READ Or GENERIC_WRITE, 0, @FSA, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL Or FILE_FLAG_OVERLAPPED, 0);
            // Success if we have a valid handle
            result := (FPipe <> INVALID_HANDLE_VALUE);
            // Need to set message mode
            If result Then Begin
                dwmode := PIPE_READMODE_MESSAGE Or PIPE_WAIT;
                SetNamedPipeHandleState(FPipe, dwmode, Nil, Nil);
                // Create thread to handle the pipe IO
                FWorker := TPipeClientThread.Create(Self, FPipe, FKillEv, FWriteQueue.DataEvent);
                FWorker.OnTerminate := RemoveWorkerThread;
            End;
        End Else
            // Failure
            result := False;

        // Set connected flag
        FConnected := result;
{$IFDEF EXTENDEDLOGGING}
        If Assigned(PipesLogProc) Then
            PipesLogProc('PipeClient.Connect Result: ' + FPipeName + IntToStr(ord(Result)));
{$ENDIF}
    Finally
        InConnect := false;
    End;
End;

Procedure TPipeClient.Disconnect(Const CanWait: Boolean);
Var
    i: Integer;
    MyWorker: TPipeClientThread;
Begin
    If Not Assigned(self) Then Begin
        If DebugHook <> 0 Then
            Raise Exception.Create('nil');
        Exit;
    End;

    // Exit if not connected
    If Not FConnected Then Begin
        If DebugHook <> 0 Then
            FConnected := false;
        Exit;
    End;

{$IFDEF EXTENDEDLOGGING}
    If Assigned(PipesLogProc) Then PipesLogProc('PipeClient.Disconnect: ' + FPipeName + ' CanWait: ' + IntToStr(ord(CanWait)));
{$ENDIF}

    // Signal the kill event
    SetEvent(FKillEv);

    // make local copy of FWorker, because FWorker is set to nil by the thread, OMFG!
    EnterCriticalSection(FCritical);
    Try
        MyWorker := FWorker;
        // this is thread safe: while we are in FCritical,
        // "RemoveWorkerThread" cannot set FWorker to nil,
        // and the worker thread cannot free itself,
        // (unless it has done so before, and then MyWorker is nil)
        // because the threading library is calling RemoveWorkerThread via Synchronize
        If Assigned(MyWorker) Then Begin
            MyWorker.FCanTerminateAndFree := false;
            Try
                MyWorker.Terminate;
            Except
            End;
        End;
    Finally
        LeaveCriticalSection(FCritical);
    End;

    // wait for the worker thread to finish
    If Assigned(MyWorker) Then Begin
        If CanWait Then Begin
            While MyWorker.ReturnValue = 0 Do Begin
                // MUST PROCESS MESSAGES OR THIS WILL HANG FOREVER
                Forms.Application.ProcessMessages;
                sleep(50);
            End;
        End;

        MyWorker.FCanTerminateAndFree := true;
        If Not MyWorker.FreeOnTerminate Then
            FreeAndNil(MyWorker);
    End;

    // Set new state
    FConnected := False;
End;

Function TPipeClient.Write(Const Buffer; Count: Integer): Boolean;
Begin
    // Set default result (depends on connected state)
    If Not Assigned(self) Then Begin
        Result := false;
        Exit;
    End;
    result := FConnected;

    // Exit if not connected
    If Not result Then
        exit;

    // Enqueue the data
    FWriteQueue.Enqueue(AllocPipeWrite(Buffer, Count));
End;

Procedure TPipeClient.SetPipeName(Const Value: String);
Begin

    // Raise error if pipe is connected
    If FConnected Then Raise EPipeException.CreateRes(PResStringRec(@resPipeConnected));

    // Check the pipe name
    CheckPipeName(Value);

    // Set the pipe name
    FPipeName := Value;

End;

Procedure TPipeClient.SetServerName(Const Value: String);
Begin

    // Raise error if pipe is connected
    If FConnected Then Raise EPipeException.CreateRes(PResStringRec(@resPipeConnected));

    // Set the server name
    FServerName := Value;

End;

Function TPipeClient.Dequeue: PPipeWrite;
Begin
    // Dequeue the record and break
    result := FWriteQueue.Dequeue;
End;

Procedure TPipeClient.RemoveWorkerThread(Sender: TObject);
Var
    LWorker: TPipeClientThread;
Begin
    If Not IsValidObject(self) Or
    Not IsValidObject(Sender) Then
        Exit;
    If (Sender As TPipeClientThread).FPipeClient = Nil Then Begin
        If isMainThread And (DebugHook <> 0) Then
            isMainThread := true;
        Exit;
    End;
    If (ord(FConnected) = 0) Then
        If Not Assigned(FWorker) Then // TG 2013
            Exit
        Else
            // OK, proceed
    Else If (ord(FConnected) <> 1) Then Begin
        If (DebugHook <> 0) Then
            Raise Exception.Create('TPipeClient already freed');
        Exit;
    End;
    // Resource protection
    Try
        // Clear the thread object first
        EnterCriticalSection(FCritical);
        Try
            LWorker := FWorker;
            FWorker := Nil;
        Finally
            LeaveCriticalSection(FCritical);
        End;
        // Call the OnPipeDisconnect if not in a destroying state
        If Not FDestroying And Assigned(FOPD) Then
            FOPD(Self, FPipe);
    Finally
        // Invalidate the pipe handle
        FPipe := INVALID_HANDLE_VALUE;
        // Set state to disconneted
        FConnected := False;
    End;

    If Assigned(LWorker) Then
        While Not LWorker.CanTerminateAndFree Do
            If isMainThread Then Begin
                LWorker.FreeOnTerminate := false; // cannot loop/hang main thread, so we risk a memory leak
                break;
            End Else
                sleep(1);
End;

Procedure TPipeClient.WaitUntilWriteQueueEmpty(Const MaxWaitSecs: Integer);
Var
    waitcounter, index: Integer;
    AllEmpty: Boolean;
Begin
    waitcounter := 0;
    Repeat
        AllEmpty := false;
        Try
            AllEmpty := FWriteQueue.GetCount = 0;
        Except
        End;
        If AllEmpty Then
            break;
        If waitcounter >= MaxWaitSecs * 20 Then
            break;
        Forms.Application.ProcessMessages;
        sleep(50);
        Inc(waitcounter);
    Until Not IsValidObject(self);
End;

Procedure TPipeClient.WndMethod(Var Message: TMessage);
Var
    MemStream: TMemoryStream;
    lpmsg: PChar;
    dwmem: Integer;
Begin
    If Not IsValidObject(self) Then
        Exit;

    // Handle the pipe messages
    Case Message.Msg Of
        WM_QUERYENDSESSION: Message.Result := 1;
        WM_PIPEERROR_W: If Assigned(FOPE) Then
                FOPE(Self, Message.wParam, pcWorker, Message.lParam);
        WM_PIPEDISCONNECT: If Assigned(FOPD) Then
                FOPD(Self, Message.wParam);
        WM_PIPESEND: If Assigned(FOPS) Then
                FOPS(Self, Message.wParam, Message.lParam);
        WM_PIPEMESSAGE: Begin
                dwmem := GlobalSize(Message.lParam);
                If (dwmem > 0) Then Begin
                    MemStream := TMemoryStream.Create;
                    lpmsg := GlobalLock(Message.lParam);
                    Try
                        // Copy the actual stream contents over
                        MemStream.Write(lpmsg^, dwmem);
                        MemStream.Position := 0;
                        // Call the OnMessage event if assigned
                        If Assigned(FOPM) Then
                            FOPM(Self, Message.wParam, MemStream);
                    Finally
                        MemStream.Free;
                        GlobalUnLock(Message.lParam);
                    End;
                End;
                GlobalFree(Message.lParam);
            End;
    Else
        // Call default window procedure
        DefWindowProc(FHwnd, Message.Msg, Message.wParam, Message.lParam);
    End;

End;

Function TPipeClient.Busy: Boolean;
Begin
    Busy := (FWriteQueue.GetCount > 0) Or
    Assigned(FWorker) And Not FWorker.Terminated;
End;

////////////////////////////////////////////////////////////////////////////////
//
//   TPipeClientThread
//
////////////////////////////////////////////////////////////////////////////////
Constructor TPipeClientThread.Create(PipeClient: TPipeClient; Pipe: HPIPE; KillEvent, DataEvent: THandle);
Begin
    // Set starting parameters
    FreeOnTerminate := True;
    FPipe := Pipe;
    FPipeClient := PipeClient;
    FNotify := PipeClient.WindowHandle;
    FErrorCode := ERROR_SUCCESS;
    FPendingRead := False;
    FPendingWrite := False;
    FPipeWrite := Nil;
    FRcvSize := MIN_BUFFER;
    FRcvBuffer := AllocMem(FRcvSize);
    FRcvStream := TMemoryStream.Create;
    FCanTerminateAndFree := true;
    FOlapRead.Offset := 0;
    FOlapRead.OffsetHigh := 0;
    FOlapRead.hEvent := CreateEvent(Nil, True, False, Nil);
    FOlapWrite.hEvent := CreateEvent(Nil, True, False, Nil);
    ResetEvent(KillEvent);
    FEvents[0] := KillEvent;
    FEvents[1] := FOlapRead.hEvent;
    FEvents[2] := FOlapWrite.hEvent;
    FEvents[3] := DataEvent;

    // Perform inherited
    If (PipeClient.PipeName = '') Then
        If DebugHook <> 0 Then
            Raise Exception.Create('PipeClient.Name=''''');
    Inc(PCTNUM);
    Inherited Create(False{$IFDEF USETGTHREADS},
        'PipeClientThread #' + SysUtils.IntToStr(PCTNUM) + ' for ' + PipeClient.PipeName{$ENDIF});
    AddValidObject(self);
End;

Destructor TPipeClientThread.Destroy;
Begin
    If Not Assigned(self) Or Not IsValidObject(self) Then
        Exit;

    RemoveValidObject(self);
    // Free the write buffer we may be holding on to
    If FPendingWrite And Assigned(FPipeWrite) Then DisposePipeWrite(FPipeWrite);

    // Free the receiver stream and buffer memory
    FreeMem(FRcvBuffer);
    FRcvStream.Free;

    // Perform inherited
    Inherited Destroy;
End;

Function TPipeClientThread.QueuedRead: Boolean;
Var
    bRead: Boolean;
Begin

    // Set default result
    result := True;

    // If we already have a pending read then nothing to do
    If Not (FPendingRead) Then Begin
        // Set defaults for reading
        FRcvStream.Clear;
        FRcvSize := MIN_BUFFER;
        ReAllocMem(FRcvBuffer, FRcvSize);
        // Keep reading all available data until we get a pending read or a failure
        While IsValidObject(self) And result And Not (FPendingRead) Do Begin
            // Perform a read
            bRead := ReadFile(FPipe, FRcvBuffer^, FRcvSize, FRcvRead, @FOlapRead);
            // Get the last error code
            FErrorCode := GetLastError;
            // Check the read result
            If bRead Then Begin
{$IFDEF EXTENDEDLOGGING}
                If Assigned(PipesLogProc) Then
                    PipesLogProc('Pipe Client Small Msg Received');
{$ENDIF}
                // We read a full message
                FRcvStream.Write(FRcvBuffer^, FRcvRead);
                // Call the OnData
                DoMessage;
            End Else Begin
{$IFDEF EXTENDEDLOGGING}
                If Assigned(PipesLogProc) Then
                    PipesLogProc('Pipe Client Msg Received, Result Code=' + IntToStr(FErrorCode));
{$ENDIF}
                // Handle cases where message is larger than read buffer used
                If (FErrorCode = ERROR_MORE_DATA) Then Begin
                    // Write the current data
                    FRcvStream.Write(FRcvBuffer^, FRcvSize);
                    // Determine how much we need to expand the buffer to
                    If PeekNamedPipe(FPipe, Nil, 0, Nil, Nil, @FRcvSize) Then Begin
{$IFDEF EXTENDEDLOGGING}
                        If Assigned(PipesLogProc) Then
                            PipesLogProc('Pipe Client Buf Size needs to be ' + IntToStr(FRcvSize));
{$ENDIF}
                        ReallocMem(FRcvBuffer, FRcvSize);
                    End Else Begin
                        // Failure
                        FErrorCode := GetLastError;
{$IFDEF EXTENDEDLOGGING}
                        If Assigned(PipesLogProc) Then
                            PipesLogProc('Pipe Client Peek Result Code=' + IntToStr(FErrorCode));
{$ENDIF}
                        result := False;
                    End;
                End
                    // Pending read
                Else If (FErrorCode = ERROR_IO_PENDING) Then
                    // Set pending flag
                    FPendingRead := True
                Else
                    // Failure
                    result := False;
            End;
        End;
    End;
End;

Function TPipeClientThread.CompleteRead: Boolean;
Begin

    // Reset the read event and pending flag
    ResetEvent(FOlapRead.hEvent);

    // Check the overlapped results
    result := GetOverlappedResult(FPipe, FOlapRead, FRcvRead, True);

    // Handle failure
    If Not (result) Then Begin
        // Get the last error code
        FErrorCode := GetLastError;
        // Check for more data
        If (FErrorCode = ERROR_MORE_DATA) Then Begin
            // Write the current data
            FRcvStream.Write(FRcvBuffer^, FRcvSize);
            // Determine how much we need to expand the buffer to
            result := PeekNamedPipe(FPipe, Nil, 0, Nil, Nil, @FRcvSize);
            If result Then Begin
                // Realloc mem to read in rest of message
                ReallocMem(FRcvBuffer, FRcvSize);
                // Read from the file again
                result := ReadFile(FPipe, FRcvBuffer^, FRcvSize, FRcvRead, @FOlapRead);
                // Handle error
                If Not (result) Then Begin
                    // Set error code
                    FErrorCode := GetLastError;
                    // Check for pending again, which means our state hasn't changed
                    If (FErrorCode = ERROR_IO_PENDING) Then Begin
                        // Bail out and wait for this operation to complete
                        result := True;
                        exit;
                    End;
                End;
            End Else
                // Set error code
                FErrorCode := GetLastError;
        End;
    End;

    // Handle success
    If result Then Begin
        // We read a full message
        FRcvStream.Write(FRcvBuffer^, FRcvRead);
        // Call the OnData
        DoMessage;
        // Reset the pending read
        FPendingRead := False;
    End;

End;

Function TPipeClientThread.QueuedWrite: Boolean;
Var
    bWrite: Boolean;
Begin
    // Set default result
    result := True;

    // If we already have a pending write then nothing to do
    If Not (FPendingWrite) Then Begin
        // Check state of data event
        If (WaitForSingleObject(FEvents[3], 0) = WAIT_OBJECT_0) Then Begin
            // Pull the data from the queue
            DoDequeue; // now thread safe, no more Synchronize call necessary
            // Is the record assigned?
            If Assigned(FPipeWrite) Then Begin
                // Write the data to the client
                bWrite := WriteFile(FPipe, FPipeWrite^.Buffer^, FPipeWrite^.Count, FWrite, @FOlapWrite);
                // Get the last error code
                FErrorCode := GetLastError;
                // Check the write operation
                If bWrite Then Begin
                    // Call the OnData in the main thread
                    PostMessage(FNotify, WM_PIPESEND, FPipe, FWrite);
                    // Free the pipe write data
                    DisposePipeWrite(FPipeWrite);
                    FPipeWrite := Nil;
                    // Reset the write event
                    ResetEvent(FOlapWrite.hEvent);
                End Else Begin
                    // Only acceptable error is pending
                    If (FErrorCode = ERROR_IO_PENDING) Then
                        // Set pending flag
                        FPendingWrite := True
                    Else
                        // Failure
                        result := False;
                End;
            End;
        End;
    End;

End;

Function TPipeClientThread.CompleteWrite: Boolean;
Begin

    // Reset the write event and pending flag
    ResetEvent(FOlapWrite.hEvent);

    // Check the overlapped results
    result := GetOverlappedResult(FPipe, FOlapWrite, FWrite, True);

    // Handle failure
    If Not (result) Then
        // Get the last error code
        FErrorCode := GetLastError
    Else
        // We sent a full message so call the OnSent in the main thread
        PostMessage(FNotify, WM_PIPESEND, FPipe, FWrite);

    // We are done either way. Make sure to free the queued pipe data
    // and to reset the pending flag
    If Assigned(FPipeWrite) Then Begin
        DisposePipeWrite(FPipeWrite);
        FPipeWrite := Nil;
    End;
    FPendingWrite := False;

End;

Procedure TPipeClientThread.DoDequeue;
Begin
    // Get the next queued data event
    If Assigned(FPipeClient) Then
        FPipeWrite := FPipeClient.Dequeue
    Else
        FPipeWrite := Nil;
End;

Procedure TPipeClientThread.DoMessage;
Var
    lpmem: THandle;
    lpmsg: PChar;
Begin

    // Convert the memory to global memory and send to pipe server
    lpmem := GlobalAlloc(GHND, FRcvStream.Size);
    lpmsg := GlobalLock(lpmem);

    // Copy from the pipe
    FRcvStream.Position := 0;
    FRcvStream.Read(lpmsg^, FRcvStream.Size);

    // Unlock the memory
    GlobalUnlock(lpmem);

    // Send to the pipe server to manage
    PostMessage(FNotify, WM_PIPEMESSAGE, FPipe, lpmem);

    // Clear the read stream
    FRcvStream.Clear;

End;

Procedure TPipeClientThread.Execute;
Var
    dwEvents: Integer;
    bOK: Boolean;
Begin
    ReturnValue := 0;
    If Assigned(FPipeClient) Then
        FPipeClient.ReturnValue := 0;
    // Loop while not terminated
    While IsValidObject(self) And Not (Terminated) Do Begin
        // Make sure we always have an outstanding read and write queued up
        bOK := (QueuedRead And QueuedWrite);
        If bOK Then Begin
            // If we are in a pending write then we need will not wait for the
            // DataEvent, because we are already waiting for a write to finish
            dwEvents := 4;
            If FPendingWrite Then Dec(dwEvents);
            // Handle the event that was signalled (or failure)
            Case WaitForMultipleObjects(dwEvents, PWOHandleArray(@FEvents), False, INFINITE) Of
                // Killed by pipe server
                WAIT_OBJECT_0: Terminate;
                // Read completed
                WAIT_OBJECT_0 + 1: bOK := CompleteRead;
                // Write completed
                WAIT_OBJECT_0 + 2: bOK := CompleteWrite;
                // Data waiting to be sent
                WAIT_OBJECT_0 + 3: ; // Data available to write
            Else
                // General failure
                FErrorCode := GetLastError;
                bOK := False;
            End;
        End;
        // Check status
        If Not (bOK) Then Begin
            // Call OnError in the main thread if this is not a disconnect. Disconnects
            // have their own event, and are not to be considered an error
            If (FErrorCode <> ERROR_PIPE_NOT_CONNECTED) Then
                PostMessage(FNotify, WM_PIPEERROR_W, FPipe, FErrorCode);
            // Terminate
            Terminate;
        End;
    End;

    // Make sure the handle is still valid
    If (FErrorCode <> ERROR_INVALID_HANDLE) Then Begin
        DisconnectNamedPipe(FPipe);
        CloseHandle(FPipe);
    End;

    // Close all open handles that we own
    CloseHandle(FOlapRead.hEvent);
    CloseHandle(FOlapWrite.hEvent);

    ReturnValue := 1;
    If Assigned(FPipeClient) Then
        FPipeClient.ReturnValue := 1;
    While Not CanTerminateAndFree Do
        sleep(10);
End;

End.

