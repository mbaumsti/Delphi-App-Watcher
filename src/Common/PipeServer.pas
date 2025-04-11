(*******************************************************************************
  Project  : AppWatcher
  Unit     : PipeServer
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

Unit PipeServer;

Interface

Uses
    PipesCommon,
    Forms, Windows, SysUtils, Classes, Messages, SyncObjs;

Type

    // Pipe info record stored by the TPipeServer component for each working thread
    PPipeInfo = ^TPipeInfo;
    TPipeInfo = Packed Record
        Pipe: HPIPE;
        WriteQueue: TWriteQueue;
    End;

    // Forward declarations
    TPipeServer = Class;

    // Pipe Listen thread class
    TPipeListenThread = Class(TPipeThread)
    public
        // Private declarations
        FNotify: HWND;
        FErrorCode: Integer;
        FPipe: HPIPE;
        FPipeName: String;
        FConnected: Boolean;
        FEvents: Array[0..1] Of THandle;
        FOlapConnect: TOverlapped;
        FPipeServer: TPipeServer;
        FSA: TSecurityAttributes;
    protected
        // Protected declarations
        Function CreateServerPipe(Const reopening: Boolean): Boolean;
        Procedure DoWorker;
        Procedure Execute; override;
    public
        // Public declarations
        FullPipeName: String;
        Constructor Create(PipeServer: TPipeServer; KillEvent: THandle);
        Destructor Destroy; override;
    End;

    // Pipe Server component
    TPipeServer = Class(TObject)
    private
        // Private declarations
        FHwnd: HWND;
        FPipeName: String;
        FActive: Boolean;
        FInShutDown: Boolean;
        FKillEv: THandle;
        FClients: TList;
        FListener: TPipeListenThread;
        FMustBeFirstInstance: Boolean;
        FThreadCount: Integer;
        FCritical: TRTLCriticalSection;
        FSA: TSecurityAttributes;
        FOPS: TOnPipeSent;
        FOPC: TOnPipeConnect;
        FOPD: TOnPipeDisconnect;
        FOPM: TOnPipeMessage;
        FOPE: TOnPipeError;
        FOnStopped: TOnServerStopped;
        Procedure DoStartup;
        Procedure DoShutdown;
    protected
        // Protected declarations
        Function Dequeue(Pipe: HPIPE): PPipeWrite;
        Function GetClient(Index: Integer): HPIPE;
        Function GetClientCount: Integer;
        Procedure WndMethod(Var Message: TMessage);
        Procedure RemoveClient(Pipe: HPIPE);
        Procedure SetActive(Value: Boolean);
        Procedure SetPipeName(Const Value: String);
        Procedure AddWorkerThread(Pipe: HPIPE);
        Procedure RemoveWorkerThread(Sender: TObject);
        Procedure RemoveListenerThread(Sender: TObject);
    public
        // Public declarations
        Constructor Create;
        Destructor Destroy; override;
        Function Write(Pipe: HPIPE; Const Buffer; Count: Integer): Boolean;
        Function GetFullPipeName: String;
        Property WindowHandle: HWND Read FHwnd;
        Property ClientCount: Integer Read GetClientCount;
        Property Clients[Index: Integer]: HPIPE Read GetClient;
        Function Broadcast(Const Buffer; Count: Integer): Boolean;
        Function FindClientNumForPipe(Const p: HPIPE): Integer;
        Procedure ClearWriteQueues;
        Procedure WaitUntilWriteQueuesEmpty(Const MaxWaitSecs: Integer);
        Procedure LimitWriteQueues(Const Limit: Integer);
        Function PipeCreatedOK: Boolean;
        Procedure WaitUntilActive(Const ms: Integer);
        Property Active: Boolean Read FActive Write SetActive;
        Property OnPipeSent: TOnPipeSent Read FOPS Write FOPS;
        Property OnPipeConnect: TOnPipeConnect Read FOPC Write FOPC;
        Property OnPipeDisconnect: TOnPipeDisconnect Read FOPD Write FOPD;
        Property OnPipeMessage: TOnPipeMessage Read FOPM Write FOPM;
        Property OnPipeError: TOnPipeError Read FOPE Write FOPE;
        Property OnServerStopped: TOnServerStopped Read FOnStopped Write FOnStopped;
        Property PipeName: String Read FPipeName Write SetPipeName;
        Property MustBeFirstInstance: Boolean Read FMustBeFirstInstance Write FMustBeFirstInstance;
        Property Listener: TPipeListenThread Read FListener;
    End;

    // Pipe Server worker thread class
    TPipeServerThread = Class(TPipeThread)
    private
        // Private declarations
        FNotify: HWND;
        FPipe: HPIPE;
        FErrorCode: Integer;
        FPipeServer: TPipeServer;
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
        Constructor Create(PipeServer: TPipeServer; Pipe: HPIPE; KillEvent, DataEvent: THandle);
        Destructor Destroy; override;
        Property Pipe: HPIPE Read FPipe;
    End;

Implementation

////////////////////////////////////////////////////////////////////////////////
//
//   TServerPipe
//
////////////////////////////////////////////////////////////////////////////////
Procedure TPipeServer.ClearWriteQueues;
Var
    index: Integer;
    ppiClient: PPipeInfo;
Begin
    EnterCriticalSection(FCritical);
    Try
        For index := FClients.Count - 1 Downto 0 Do Begin
            Try
                ppiClient := FClients[index];
                ppiClient.WriteQueue.Clear;
            Except
            End;
        End;
    Finally
        LeaveCriticalSection(FCritical);
    End;
End;

Procedure TPipeServer.WaitUntilActive(Const ms: Integer);
Var
    i: Integer;
Begin
    i := 0;
    While Not PipeCreatedOK Do Begin
        Forms.Application.ProcessMessages;
        sleep(50);
        Inc(i);
        If i * 50 > ms Then
            break;
        Forms.Application.ProcessMessages;
    End;
End;

Procedure TPipeServer.WaitUntilWriteQueuesEmpty(Const MaxWaitSecs: Integer);
Var
    waitcounter, index: Integer;
    ppiClient: PPipeInfo;
    AllEmpty: Boolean;
Begin
    waitcounter := 0;
    Repeat
        AllEmpty := true;
        EnterCriticalSection(FCritical);
        Try
            For index := FClients.Count - 1 Downto 0 Do Begin
                Try
                    ppiClient := FClients[index];
                    If ppiClient.WriteQueue.GetCount > 0 Then Begin
                        AllEmpty := false;
                        break;
                    End;
                Except
                End;
            End;
        Finally
            LeaveCriticalSection(FCritical);
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

Constructor TPipeServer.Create;
Begin
    // Perform inherited
    Inherited Create;

    If Not isMainThread Then
        Raise Exception.Create('Must be in main thread to create a TPipeServer.');

    InitializeCriticalSection(FCritical);
{$IFDEF EXTENDEDLOGGING}
    If Assigned(PipesLogProc) Then PipesLogProc('TPipeServer.Create');
{$ENDIF}

    // Initialize the security attributes
    InitializeSecurity(FSA);

    // Set staring defaults
    FPipeName := 'PipeServer';
    FActive := False;
    FInShutDown := False;
    FKillEv := CreateEvent(@FSA, True, False, Nil);
    FClients := TList.Create;
    FListener := Nil;
    FThreadCount := 0;
    FHwnd := AllocateHWnd(WndMethod);
    AddValidObject(self);
End;

Destructor TPipeServer.Destroy;
Begin
    If Not isMainThread Then
        Raise Exception.Create('Must be in main thread to destroy a TPipeServer  (Pipe:' + FPipeName + ').');

    If Not Assigned(self) Or Not IsValidObject(self) Then
        Exit;

    // Perform the shutdown if active
    If FActive Then
        DoShutdown;

{$IFDEF EXTENDEDLOGGING}
    If Assigned(PipesLogProc) Then PipesLogProc('TPipeServer.Destroy ' + FPipeName);
{$ENDIF}

    // Release all objects, events, and handles
    CloseHandle(FKillEv);
    EnterCriticalSection(FCritical);
    Try
        FreeAndNil(FClients);
    Finally
        LeaveCriticalSection(FCritical);
    End;
    FinalizeSecurity(FSA);

    // Close the window
    If FHwnd <> 0 Then
        DeAllocateHWnd(FHwnd);
    FHwnd := 0;
    RemoveValidObject(self);
    DeleteCriticalSection(FCritical);

    // Perform inherited
    Inherited Destroy;
End;

Procedure TPipeServer.WndMethod(Var Message: TMessage);
Var
    MemStream: TMemoryStream;
    lpmsg: PChar;
    dwmem: Integer;
Begin
    If Not IsValidObject(self) Then
        Exit;
    Case Message.Msg Of
        WM_QUERYENDSESSION: Message.Result := 1;
        WM_PIPEERROR_L:
            If Not (FInShutdown) And Assigned(FOPE) Then
                FOPE(Self, Message.wParam, pcListener, Message.lParam);
        WM_PIPEERROR_W:
            If Not (FInShutdown) And Assigned(FOPE) Then
                FOPE(Self, Message.wParam, pcWorker, Message.lParam);
        WM_PIPECONNECT:
            If Assigned(FOPC) Then
                FOPC(Self, Message.wParam);
        WM_PIPEDISCONNECT:
            If Assigned(FOPD) Then
                FOPD(Self, Message.wParam);
        WM_PIPESEND:
            If Assigned(FOPS) Then
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
        DefWindowProc(FHwnd, Message.Msg, Message.wParam, Message.lParam);
    End;

End;

Function TPipeServer.GetClient(Index: Integer): HPIPE;
Begin
    Result := 0;
    // Return the requested pipe
    EnterCriticalSection(FCritical);
    Try
        If Assigned(FClients) And
        (Index < FClients.Count) Then
            result := PPipeInfo(FClients[Index])^.Pipe;
    Finally
        LeaveCriticalSection(FCritical);
    End;
End;

Function TPipeServer.GetClientCount: Integer;
Begin
    // Return the number of active clients
    EnterCriticalSection(FCritical);
    Try
        If Not Assigned(self) Or
        Not Assigned(FClients) Then
            Result := 0
        Else
            result := FClients.Count;
    Finally
        LeaveCriticalSection(FCritical);
    End;
End;

Function TPipeServer.GetFullPipeName: String;
Begin
    If Assigned(FListener) Then
        Result := FListener.FullPipeName
    Else
        Result := '';
End;

Procedure TPipeServer.LimitWriteQueues(Const Limit: Integer);
Var
    index: Integer;
    ppiClient: PPipeInfo;
    pw: PPipeWrite;
Begin
    EnterCriticalSection(FCritical);
    Try
        For index := FClients.Count - 1 Downto 0 Do Begin
            // Get the pipe record and compare handles
            Try
                ppiClient := FClients[index];
                While ppiClient.WriteQueue.GetCount > Limit Do Begin
                    pw := ppiClient.WriteQueue.Dequeue;
{$IFDEF EXTENDEDLOGGING}
                    If Assigned(PipesLogProc) Then
                        PipesLogProc('Skipped Sending Data: ' +
                            IntToStr(pw.Count) + ' bytes.');
{$ENDIF}
                    DisposePipeWrite(pw);
                End;
            Except
                // esp. ignore if FClients[index] does not exist due to thread concurrency
            End;
        End;
    Finally
        LeaveCriticalSection(FCritical);
    End;
End;

Function TPipeServer.PipeCreatedOK: Boolean;
Begin
    PipeCreatedOK := Assigned(self) And Active And Assigned(FListener) And (FListener.FPipe <> INVALID_HANDLE_VALUE);
End;

Function TPipeServer.Write(Pipe: HPIPE; Const Buffer; Count: Integer): Boolean;
Var
    index: Integer;
    ppiClient: PPipeInfo;
Begin
{$IFDEF MEMDEBUG}KMSetThreadMarker('PipSrvW0');
{$ENDIF}
    // Set default result
    result := False;

{$IFDEF EXTENDEDLOGGING}
    If Assigned(PipesLogProc) Then
        PipesLogProc('Write to pipe ' +
            IntToStr(Pipe) + ': ' + IntToStr(Count) +
            ' Bytes, ClientCount=' + IntToStr(ClientCount));
{$ENDIF}
    // Locate the pipe info record for the given pipe first
    ppiClient := Nil;
    EnterCriticalSection(FCritical);
    Try
        For index := FClients.Count - 1 Downto 0 Do Begin
            // Get the pipe record and compare handles
            Try
                ppiClient := FClients[index];
                If (ppiClient^.Pipe = Pipe) Then
                    break;
                ppiClient := Nil;
            Except
            End;
        End;

{$IFDEF EXTENDEDLOGGING}
        If Not Assigned(ppiClient) Then
            If Assigned(PipesLogProc) Then
                PipesLogProc('not found pipe ' +
                    IntToStr(Pipe) + ': ' + IntToStr(Count) +
                    ' Bytes, ClientCount=' + IntToStr(ClientCount));
{$ENDIF}
        // If client record is nil then raise exception
        If (ppiClient = Nil) Or (Count > MAX_BUFFER) Then exit;

{$IFDEF MEMDEBUG}KMSetThreadMarker('PipSrvW5');
{$ENDIF}
        // Queue the data
        ppiClient.WriteQueue.Enqueue(AllocPipeWrite(Buffer, Count));

    Finally
        LeaveCriticalSection(FCritical);
    End;

{$IFDEF EXTENDEDLOGGING}If Assigned(PipesLogProc) Then PipesLogProc('PipeServer Enqueuing Data: ' + IntToStr(Count) + ' bytes.');
{$ENDIF}
    result := True;
{$IFDEF MEMDEBUG}KMSetThreadMarker('PipSrvWX');
{$ENDIF}
End;

Function TPipeServer.Broadcast(Const Buffer; Count: Integer): Boolean;
Var
    dwCount: Integer;
Begin
{$IFDEF MEMDEBUG}KMSetThreadMarker('PipSrvB0');
{$ENDIF}
    // Default result
    result := True;

{$IFDEF EXTENDEDLOGGING}
    If Assigned(PipesLogProc) Then
        PipesLogProc('Broadcast: ' + IntToStr(Count) +
            ' Bytes, ClientCount=' + IntToStr(ClientCount));
{$ENDIF}

    // Iterate the pipes and write to each one. *** Added by Russell on 01.19.2004 ***

    EnterCriticalSection(FCritical);
    Try
        For dwCount := Pred(ClientCount) Downto 0 Do Begin
            Try
                // Fail if a write fails
{$IFDEF MEMDEBUG}KMSetThreadMarker('PipSrvB1');
{$ENDIF}
                result := Write(Clients[dwCount], Buffer, Count);
{$IFDEF MEMDEBUG}KMSetThreadMarker('PipSrvB2');
{$ENDIF}
                // Break on a failed write
                If Not result Then
                    break;
            Except
            End;
        End;
    Finally
        LeaveCriticalSection(FCritical);
    End;

{$IFDEF MEMDEBUG}KMSetThreadMarker('PipSrvBX');
{$ENDIF}
End;

Function TPipeServer.Dequeue(Pipe: HPIPE): PPipeWrite;
Var
    index: Integer;
    ppiClient: PPipeInfo;
Begin
    // Locate the pipe info record for the given pipe and dequeue the next
    // available record
    result := Nil;
    EnterCriticalSection(FCritical);
    Try
        For index := FClients.Count - 1 Downto 0 Do Try
                // Get the pipe record and compare handles
                ppiClient := FClients[index];
                If (ppiClient^.Pipe = Pipe) Then Begin
                    // Found the desired pipe record, dequeue the record and break
                    result := ppiClient.WriteQueue.Dequeue;
                    break;
                End;
            Except
            End;
    Finally
        LeaveCriticalSection(FCritical);
    End;
End;

Procedure TPipeServer.RemoveClient(Pipe: HPIPE);
Var
    index: Integer;
    ppiClient: PPipeInfo;
Begin
{$IFDEF EXTENDEDLOGGING}
    If Assigned(PipesLogProc) Then
        PipesLogProc('PipeServer.Disconnect pipe ' + IntToStr(Pipe));
{$ENDIF}

    // Locate the pipe info record for the give pipe and remove it
    EnterCriticalSection(FCritical);
    Try
        For index := FClients.Count - 1 Downto 0 Do Try
                // Get the pipe record and compare handles
                ppiClient := FClients[index];
                If (ppiClient^.Pipe = Pipe) Then Begin
                    // Found the desired pipe record, free it
                    FClients.Delete(index);
                    ppiClient.WriteQueue.Free;
                    FreeMem(ppiClient);
                    // Call the OnDisconnect if assigned and not in a shutdown
                    If Not (FInShutdown) And Assigned(FOPD) Then PostMessage(FHwnd, WM_PIPEDISCONNECT, Pipe, 0);
                    // Break the loop
                    break;
                End;
            Except
            End;
    Finally
        LeaveCriticalSection(FCritical);
    End;
End;

Procedure TPipeServer.SetActive(Value: Boolean);
Begin
    If Not Assigned(self) Then
        Exit;
    // Check against current state
    If Assigned(PipesLogProc) Then
        PipesLogProc('TPipeServer.SetActive(' + Bool2S(Value) + ')');
    If (FActive <> Value) Then Begin
        // Shutdown if active
        If FActive Then DoShutdown;
        // Startup if not active
        If Value Then
            DoStartup
    End;

End;

Procedure TPipeServer.SetPipeName(Const Value: String);
Begin

    // Cannot change pipe name if pipe server is active
    If FActive Then Raise EPipeException.CreateRes(PResStringRec(@resPipeActive));

    // Check the pipe name
    CheckPipeName(Value);

    // Set the new pipe name
    FPipeName := Value;
End;

Procedure TPipeServer.AddWorkerThread(Pipe: HPIPE);
Var
    ppInfo: PPipeInfo;
    pstWorker: TPipeServerThread;
Begin
{$IFDEF EXTENDEDLOGGING}
    If Assigned(PipesLogProc) Then PipesLogProc('TPipeServer.AddWorkerThread ' + FPipeName + ' for pipe #' + IntToStr(Pipe));
{$ENDIF}

    // If there are more than 100 worker threads then we need to
    // suspend the listener thread until our count decreases
    If (FThreadCount > MAX_THREADS) And
    Assigned(FListener) Then
        FListener.Suspend;

    // Create a new pipe info structure to manage the pipe
    ppInfo := AllocMem(SizeOf(TPipeInfo));
    ppInfo^.Pipe := Pipe;
    ppInfo^.WriteQueue := TWriteQueue.Create;

    // Add the structure to the list of pipes
    EnterCriticalSection(FCritical);
    Try
        FClients.Add(ppInfo);
    Finally
        LeaveCriticalSection(FCritical);
    End;

    // Resource protection
    pstWorker := Nil;
    Try
        // Create the server worker thread
        Inc(FThreadCount);
        pstWorker := TPipeServerThread.Create(Self, Pipe, FKillEv, ppInfo^.WriteQueue.DataEvent);
        pstWorker.OnTerminate := RemoveWorkerThread;
    Except
{$IFDEF EXTENDEDLOGGING}
        If Assigned(PipesLogProc) Then PipesLogProc('TPipeServer.AddWorkerThread exception');
{$ENDIF}
        // Exception during thread create, remove the client record
        RemoveClient(Pipe);
        // Disconnect and close the pipe handle
        DisconnectNamedPipe(Pipe);
        CloseHandle(Pipe);
        FreeAndNil(pstWorker);
        // Decrement the thread count
        Dec(FThreadCount);
    End;
{$IFDEF EXTENDEDLOGGING}
    If Assigned(PipesLogProc) Then PipesLogProc('TPipeServer.AddWorkerThread done');
{$ENDIF}
End;

Procedure TPipeServer.RemoveWorkerThread(Sender: TObject);
Begin
{$IFDEF EXTENDEDLOGGING}
    If Assigned(PipesLogProc) Then PipesLogProc('TPipeServer.RemoveWorkerThread ' + FPipeName);
{$ENDIF}
    If Not IsValidObject(self) Or
    Not IsValidObject(Sender) Then
        Exit;
    // Remove the pipe info record associated with this thread
    RemoveClient(TPipeServerThread(Sender).Pipe);

    // Decrement the thread count
    Dec(FThreadCount);

    // *** Added shutdown check by Adam on 01.19.2004 ***
    // If there are less than the maximum worker threads then we need to
    // resume the listener thread
    If FActive And Not FInShutdown And
    (FThreadCount < MAX_THREADS) And
    Assigned(FListener) And
    FListener.Suspended Then Begin
        FListener.Resume;
    End;
End;

Procedure TPipeServer.RemoveListenerThread(Sender: TObject);
Begin
{$IFDEF EXTENDEDLOGGING}
    If Assigned(PipesLogProc) Then PipesLogProc('TPipeServer.RemoveListenerThread ' + FPipeName);
{$ENDIF}
    If Not IsValidObject(self) Then
        Exit;
    // Decrement the thread count
    Dec(FThreadCount);

    // Clear the listener *** Added by Russell on 01.19.2004 ***
    FListener := Nil;

    // If we not in a shutdown, and we were the only thread, then
    // change the active state
    If (Not (FInShutDown) And
        (FThreadCount = 0)) Then Begin
        FActive := False;
        If Assigned(FOnStopped) Then
            FOnStopped(Self);
    End;
End;

Procedure TPipeServer.DoStartup;
Begin
    If Assigned(PipesLogProc) Then
        PipesLogProc('TPipeServer.DoStartup');
    // If we are active then exit
    If FActive Then exit;

    // while (DebugHook=0) do
    // sleep(100);

    // Make sure the kill event is in a non-signaled state
    ResetEvent(FKillEv);

{$IFDEF EXTENDEDLOGGING}
    If Assigned(PipesLogProc) Then PipesLogProc('TPipeServer.DoStartup ' + FPipeName);
{$ENDIF}

    // Resource protection
    Try
        // Create the listener thread
        Inc(FThreadCount);
        If Assigned(PipesLogProc) Then
            PipesLogProc('TPipeServer.DoStartup, FThreadCount now ' + IntToStr(FThreadCount));
        FListener := TPipeListenThread.Create(Self, FKillEv);
        FListener.OnTerminate := RemoveListenerThread;
    Except
        // Exception during thread create. Decrement the thread count and
        // re-raise the exception
        FreeAndNil(FListener);
        Dec(FThreadCount);
        Raise;
    End;

    // Set active state
    FActive := True;
    If Assigned(PipesLogProc) Then
        PipesLogProc('TPipeServer.DoStartup done ' + FPipeName);
End;

Function TPipeServer.FindClientNumForPipe(Const p: HPIPE): Integer;
Var
    i: Integer;
Begin
    EnterCriticalSection(FCritical);
    Try
        For i := 0 To ClientCount Do
            If Clients[i] = p Then Begin
                Result := i;
                Exit;
            End;
        Result := -1;
    Finally
        LeaveCriticalSection(FCritical);
    End;
End;

Procedure TPipeServer.DoShutdown;
Var
    msg: TMsg;
    failsafe: Int64;
Begin
    If Assigned(PipesLogProc) Then
        PipesLogProc('TPipeServer.DoShutdown ' + FPipeName);

    If Not Assigned(self) Or Not FActive Then
        Exit;

    // Resource protection
    Try
        // Set shutdown flag
        FInShutDown := True;
        If Assigned(FListener) Then
            FListener.Terminate;
        // Signal the kill event
        SetEvent(FKillEv);
        // Wait until all threads have completed (or we hit failsafe)
        failsafe := GetTickCount64;
        While (FThreadCount > 0) Do Begin
            // Process messages (which is how the threads synchronize with our thread)
            If PeekMessage(msg, 0, 0, 0, PM_REMOVE) Then Begin
                TranslateMessage(msg);
                DispatchMessage(msg);
            End;
            If (GetTickCount64 > failsafe + 1000) Then
                break;
        End;
    Finally
        // Set active state to false
        FInShutDown := False;
        FActive := False;
    End;
{$IFDEF EXTENDEDLOGGING}
    If Assigned(PipesLogProc) Then PipesLogProc('TPipeServer.DoShutdown done ' + FPipeName);
{$ENDIF}
End;

////////////////////////////////////////////////////////////////////////////////
//
//   TPipeServerThread
//
////////////////////////////////////////////////////////////////////////////////
Constructor TPipeServerThread.Create(PipeServer: TPipeServer; Pipe: HPIPE; KillEvent, DataEvent: THandle);
Begin
{$IFDEF EXTENDEDLOGGING}
    If Assigned(PipesLogProc) Then PipesLogProc('TPipeServerThread.Create ' + PipeServer.FPipeName);
{$ENDIF}
    // Set starting parameters
    FreeOnTerminate := True;
    FPipe := Pipe;
    FPipeServer := PipeServer;
    FNotify := PipeServer.WindowHandle;
    FErrorCode := ERROR_SUCCESS;
    FPendingRead := False;
    FPendingWrite := False;
    FPipeWrite := Nil;
    FRcvSize := MIN_BUFFER;
    FRcvBuffer := AllocMem(FRcvSize);
    FRcvStream := TMemoryStream.Create;
    FOlapRead.Offset := 0;
    FOlapRead.OffsetHigh := 0;
    FOlapRead.hEvent := CreateEvent(Nil, True, False, Nil);
    FOlapWrite.hEvent := CreateEvent(Nil, True, False, Nil);
    FEvents[0] := KillEvent;
    FEvents[1] := FOlapRead.hEvent;
    FEvents[2] := FOlapWrite.hEvent;
    FEvents[3] := DataEvent;

    // Perform inherited
    Inherited Create(False{$IFDEF USETGTHREADS}, 'PipeServerThread'{$ENDIF});
    AddValidObject(self);
End;

Destructor TPipeServerThread.Destroy;
Begin
    If Not Assigned(self) Or Not IsValidObject(self) Then
        Exit;
{$IFDEF EXTENDEDLOGGING}
    If Assigned(PipesLogProc) Then PipesLogProc('TPipeServerThread.Destroy ' + FPipeServer.FPipeName);
{$ENDIF}
    RemoveValidObject(self);
    // Free the write buffer we may be holding on to
    If FPendingWrite And Assigned(FPipeWrite) Then DisposePipeWrite(FPipeWrite);

    // Free the receiver stream and buffer memory
    FreeMem(FRcvBuffer);
    FRcvStream.Free;

    // Perform inherited
    Inherited Destroy;
End;

Function TPipeServerThread.QueuedRead: Boolean;
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
                // We read a full message
                FRcvStream.Write(FRcvBuffer^, FRcvRead);
                // Call the OnData
                DoMessage;
            End Else Begin
                // Handle cases where message is larger than read buffer used
                If (FErrorCode = ERROR_MORE_DATA) Then Begin
                    // Write the current data
                    FRcvStream.Write(FRcvBuffer^, FRcvSize);
                    // Determine how much we need to expand the buffer to
                    If PeekNamedPipe(FPipe, Nil, 0, Nil, Nil, @FRcvSize) Then
                        ReallocMem(FRcvBuffer, FRcvSize)
                    Else Begin
                        // Failure
                        FErrorCode := GetLastError;
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

Function TPipeServerThread.CompleteRead: Boolean;
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

Function TPipeServerThread.QueuedWrite: Boolean;
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
{$IFDEF EXTENDEDLOGGING}If Assigned(PipesLogProc) Then PipesLogProc('PipeServer Wrote Data: ' + IntToStr(FPipeWrite^.Count) + ' bytes.');
{$ENDIF}
                    // Call the OnData in the main thread
                    PostMessage(FNotify, WM_PIPESEND, FPipe, FWrite);
                    // Free the pipe write data
                    DisposePipeWrite(FPipeWrite);
                    FPipeWrite := Nil;
                    // Reset the write event
                    ResetEvent(FOlapWrite.hEvent);
                End Else Begin
{$IFDEF EXTENDEDLOGGING}
                    If Assigned(PipesLogProc) Then
                        PipesLogProc('PipeServer Did Not Write Data: ' +
                            IntToStr(FPipeWrite^.Count) + ' bytes, err=' +
                            IntToStr(FErrorCode));
{$ENDIF}
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

Function TPipeServerThread.CompleteWrite: Boolean;
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

Procedure TPipeServerThread.DoDequeue;
Begin
    // Get the next queued data event
    FPipeWrite := FPipeServer.Dequeue(FPipe);
End;

Procedure TPipeServerThread.DoMessage;
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

Procedure TPipeServerThread.Execute;
Var
    dwEvents: Integer;
    bOK: Boolean;
Begin
    // Notify the pipe server of the connect
    PostMessage(FNotify, WM_PIPECONNECT, FPipe, 0);
{$IFDEF EXTENDEDLOGGING}
    If Assigned(PipesLogProc) Then
        PipesLogProc('Pipe Server Thread started for pipe ' + IntToStr(FPipe));
{$ENDIF}
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
            Else Begin
                    // General failure
                    FErrorCode := GetLastError;
                    bOK := False;
                End;
            End;
        End;

        // Check status
        If Not bOK Then Begin
            // Call OnError in the main thread if this is not a disconnect. Disconnects
            // have their own event, and are not to be considered an error
            If (FErrorCode <> ERROR_BROKEN_PIPE) Then
                PostMessage(FNotify, WM_PIPEERROR_W, FPipe, FErrorCode);
            // Terminate
            Terminate;
        End;
    End;

{$IFDEF EXTENDEDLOGGING}
    If Assigned(PipesLogProc) Then
        PipesLogProc('Pipe Server Thread ending for pipe ' + IntToStr(FPipe) + ', error code ' + IntToStr(FErrorCode));
{$ENDIF}

    // Disconnect and close the pipe handle at this point. This will kill the
    // overlapped events that may be attempting to access our memory blocks
    // NOTE *** Ensure that the handle is STILL valid, otherwise the ntkernel
    // will raise an exception
    If (FErrorCode <> ERROR_INVALID_HANDLE) Then Begin
        DisconnectNamedPipe(FPipe);
        CloseHandle(FPipe);
    End;

    // Close all open handles that we own
    CloseHandle(FOlapRead.hEvent);
    CloseHandle(FOlapWrite.hEvent);
End;

////////////////////////////////////////////////////////////////////////////////
//
//   TPipeListenThread
//
////////////////////////////////////////////////////////////////////////////////
Constructor TPipeListenThread.Create(PipeServer: TPipeServer; KillEvent: THandle);
Begin
    // Set starting parameters
    FreeOnTerminate := True;
    FPipeName := PipeServer.PipeName;
    FPipeServer := PipeServer;
    FNotify := PipeServer.WindowHandle;
    FEvents[0] := KillEvent;
    InitializeSecurity(FSA);
    FPipe := INVALID_HANDLE_VALUE;
    FConnected := False;
    FOlapConnect.Offset := 0;
    FOlapConnect.OffsetHigh := 0;
    FEvents[1] := CreateEvent(@FSA, True, False, Nil);
    ;
    FOlapConnect.hEvent := FEvents[1];

    // Perform inherited
    Inherited Create(False{$IFDEF USETGTHREADS}, 'PipeListenThread'{$ENDIF});
    AddValidObject(self);
End;

Destructor TPipeListenThread.Destroy;
Begin
    If Not Assigned(self) Or Not IsValidObject(self) Then
        Exit;
    RemoveValidObject(self);

    Try
        // Close the connect event handle
        CloseHandle(FOlapConnect.hEvent);

        // Disconnect and free the handle
        If (FPipe <> INVALID_HANDLE_VALUE) Then Begin
            If FConnected Then
                DisconnectNamedPipe(FPipe);
            CloseHandle(FPipe);
        End;

        // Release memory for security structure
        FinalizeSecurity(FSA);
    Except
        // really don't know what to do here
        // exceptions are really rare here
    End;

    // Perform inherited
    Inherited Destroy;
End;

Function TPipeListenThread.CreateServerPipe(Const reopening: Boolean): Boolean;
Const
    PipeMode = PIPE_TYPE_MESSAGE Or PIPE_READMODE_MESSAGE Or PIPE_WAIT;
    Instances = PIPE_UNLIMITED_INSTANCES;
    FILE_FLAG_FIRST_PIPE_INSTANCE = $00080000;
Var
    OpenMode: Integer;
Begin
    OpenMode := PIPE_ACCESS_DUPLEX Or FILE_FLAG_OVERLAPPED;

    If Not reopening And FPipeServer.FMustBeFirstInstance Then
        OpenMode := OpenMode Or FILE_FLAG_FIRST_PIPE_INSTANCE;

    // Create the outbound pipe first
    FullPipeName := '\\.\pipe\' + FPipeName;
    FPipe := CreateNamedPipe(PChar(String(FullPipeName)),
        OpenMode, PipeMode, Instances, 0, 0, 1000, @FSA);

    // Set result value based on valid handle
    If (FPipe = INVALID_HANDLE_VALUE) Then
        FErrorCode := GetLastError
    Else
        FErrorCode := ERROR_SUCCESS;

    // Success if handle is valid
    result := (FPipe <> INVALID_HANDLE_VALUE);
End;

Procedure TPipeListenThread.DoWorker;
Begin
    // Call the pipe server on the main thread to add a new worker thread
    FPipeServer.AddWorkerThread(FPipe);
End;

Procedure TPipeListenThread.Execute;
Var
    firsttime: Boolean;
    WFMO: DWORD;
Begin
    // Thread body
    firsttime := true;
    If Assigned(PipesLogProc) Then
        PipesLogProc('TPipeListenThread.Execute ' + FPipeName);
    While IsValidObject(self) And Not (Terminated) Do Begin
        // Set default state
        FConnected := False;
        // Attempt to create first pipe server instance
        If CreateServerPipe(Not firsttime) Then Begin
            If Assigned(PipesLogProc) Then
                If firsttime Then
                    PipesLogProc('Server Pipe Created')
                Else
                    PipesLogProc('Server Pipe Reopened');

            firsttime := false;
            // Connect the named pipe
            FConnected := ConnectNamedPipe(FPipe, @FOlapConnect);
            // Handle failure
            If Not (FConnected) Then Begin
                // Check the last error code
                FErrorCode := GetLastError;
                If Assigned(PipesLogProc) Then
                    PipesLogProc('Not Connected Yet, Code ' + IntToStr(FErrorCode));

                // Is pipe connected?
                If (FErrorCode = ERROR_PIPE_CONNECTED) Then
                    FConnected := True
                    // IO pending?
                Else If (FErrorCode = ERROR_IO_PENDING) Then Begin
                    // Wait for a connect or kill signal
                    WFMO := WaitForMultipleObjects(2, PWOHandleArray(@FEvents), False, INFINITE);
                    If Assigned(PipesLogProc) Then
                        PipesLogProc('WaitForMultipleObjects Result: ' + IntToStr(WFMO));
                    Case WFMO Of
                        WAIT_FAILED: Begin
                                FErrorCode := GetLastError;
                                If Assigned(PipesLogProc) Then
                                    PipesLogProc('TPipeListenThread.Execute Failed - Code: ' + IntToStr(FErrorCode));
                            End;
                        WAIT_OBJECT_0: Begin
                                If Assigned(PipesLogProc) Then
                                    PipesLogProc('TPipeListenThread.Execute Terminating');
                                Terminate;
                            End;
                        WAIT_OBJECT_0 + 1: Begin
                                FConnected := True;
                                If Assigned(PipesLogProc) Then
                                    PipesLogProc('TPipeListenThread.Execute Succeeded');
                            End;
                    End;
                End;
            End;
            // If we are not connected at this point then we had a failure
            If Not (FConnected) Then Begin
                If Assigned(PipesLogProc) Then
                    PipesLogProc('TPipeListenThread.Execute Not Connected');
                // Client may have connected / disconnected simultaneously, in which
                // case it is not an error. Otherwise, post the error message to the
                // pipe server
                If (FErrorCode <> ERROR_NO_DATA) Then
                    PostMessage(FNotify, WM_PIPEERROR_L, FPipe, FErrorCode);
                // Close the handle
                CloseHandle(FPipe);
                FPipe := INVALID_HANDLE_VALUE;
            End Else Begin
                If Assigned(PipesLogProc) Then
                    PipesLogProc('TPipeListenThread.Execute Calling SafeSynchronize(DoWorker)');
                // Notify server of connect
                SafeSynchronize(DoWorker);
            End;
        End Else Begin
            //FPipeServer.Active:=false; this causes problems
            If Assigned(PipesLogProc) Then
                PipesLogProc('TPipeListenThread.Execute breaking');
            break;
        End;
        If Assigned(PipesLogProc) Then
            PipesLogProc('TPipeListenThread.Execute looping.');
    End;

    If Assigned(PipesLogProc) Then
        PipesLogProc('TPipeListenThread.Execute end.');
End;

End.

