(*******************************************************************************
  Project  : AppWatcher
  Unit     : PipeCommon
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

Unit PipesCommon;

Interface

{$IFDEF MSWINDOWS}
{$DEFINE EXTENDEDLOGGING}
{$ENDIF}

Uses
    Windows, SysUtils, Classes, SyncObjs, Messages;

Type
{$IFDEF USETGTHREADS}
    TThreadType = TTGThread;
{$ELSE}
    TThreadType = TThread;
{$ENDIF}

Resourcestring
    // Exception resource strings
    resPipeActive = 'Cannot change property while server is active!';
    resPipeConnected = 'Cannot change property when client is connected!';
    resBadPipeName = 'Invalid pipe name specified!';

Const

    // Maximum and minimum constants
    MAX_THREADS = 1001; // 1 Listener and 1000 Workers
    MIN_BUFFER = 4096;
    MAX_BUFFER = 100 * 1024 * 1024;

Const

    // Pipe window messages
    WM_PIPEERROR_L = WM_USER + 100;
    WM_PIPEERROR_W = Succ(WM_PIPEERROR_L);
    WM_PIPECONNECT = Succ(WM_PIPEERROR_W);
    WM_PIPEDISCONNECT = Succ(WM_PIPECONNECT);
    WM_PIPESEND = Succ(WM_PIPEDISCONNECT);
    WM_PIPEMESSAGE = Succ(WM_PIPESEND);

Const
    PipesLogProc: Procedure(a: String) = Nil;

Type
    // Define the pipe data type
    HPIPE = THandle;

    // Pipe exceptions
    EPipeException = Class(Exception);

    // Pipe context for error messages
    TPipeContext = (pcListener, pcWorker);

    // Pipe Events
    TOnPipeConnect = Procedure(Sender: TObject; Pipe: HPIPE) Of Object;
    TOnPipeDisconnect = Procedure(Sender: TObject; Pipe: HPIPE) Of Object;
    TOnPipeMessage = Procedure(Sender: TObject; Pipe: HPIPE; Stream: TStream) Of Object;
    TOnPipeSent = Procedure(Sender: TObject; Pipe: HPIPE; Size: DWORD) Of Object;
    TOnPipeError = Procedure(Sender: TObject; Pipe: HPIPE; PipeContext: TPipeContext; ErrorCode: Integer) Of Object;
    TOnServerStopped = Procedure(Sender: TObject) Of Object;

    // Pipe write data structure
    PPipeWrite = ^TPipeWrite;
    TPipeWrite = Packed Record
        Buffer: PChar;
        Count: Integer;
    End;

    // Writer queue node structure
    PWriteNode = ^TWriteNode;
    TWriteNode = Packed Record
        PipeWrite: PPipeWrite;
        NextNode: PWriteNode;
    End;

    // Writer queue class
    TWriteQueue = Class(TObject)
    private
        // Private declarations
        FDataEv: THandle;
        FHead: PWriteNode;
        FTail: PWriteNode;

        FCritical: TRTLCriticalSection;
    protected
        // Protected declarations
        Function NewNode(PipeWrite: PPipeWrite): PWriteNode;
    public
        // Public declarations
        Procedure Clear;
        Constructor Create;
        Destructor Destroy; override;
        Procedure Enqueue(PipeWrite: PPipeWrite);
        Function Dequeue: PPipeWrite;
        Property DataEvent: THandle Read FDataEv;
        Function GetCount: Integer;
    End;

    // Base class pipe thread that has a SafeSynchronize method
    TPipeThread = Class(TThread)
    protected
        Procedure SafeSynchronize(Method: TThreadMethod);
    End;

Function Stream2String(Const AStream: TStream; Const SetCPUTF8: Boolean = true): RawByteString;

Function AllocPipeWrite(Const Buffer; Count: Integer): PPipeWrite;
Procedure DisposePipeWrite(PipeWrite: PPipeWrite);
Procedure CheckPipeName(Value: String);
Procedure InitializeSecurity(Var SA: TSecurityAttributes);
Procedure FinalizeSecurity(Var SA: TSecurityAttributes);

Function IsValidObject(Const p: Pointer): Boolean;
Procedure AddValidObject(Const p: Pointer);
Procedure RemoveValidObject(Const p: Pointer);
Function Bool2S(Const b: Boolean): String;

Threadvar
    isMainThread: Boolean;
Var
    PCTNUM: Integer;

Implementation

Var
    ValidObjects: TList;
    VOCrit: TCriticalSection;
    InvalidCounter: Integer;

Function Stream2String(Const AStream: TStream; Const SetCPUTF8: Boolean = true): RawByteString;
Var
    p: Int64;
Begin
    p := AStream.Position;
    AStream.Position := 0;
    SetLength(Result, AStream.Size);
    If AStream.Size > 0 Then
        AStream.Read(Result[1], AStream.Size);
    AStream.Position := p;
    If SetCPUTF8 Then
        System.SetCodePage(Result, CP_UTF8, False);
End;

Procedure AddValidObject(Const p: Pointer);
Begin
    If Not Assigned(ValidObjects) Then
        Exit;
    VOCrit.Enter;
    Try
        ValidObjects.Add(p);
    Finally
        VOCrit.Leave;
    End;
End;

Function IsValidObject(Const p: Pointer): Boolean;
Begin
    Result := false;
    If Not Assigned(p) Or Not Assigned(ValidObjects) Then
        Exit;
    VOCrit.Enter;
    Try
        Result := ValidObjects.IndexOf(p) >= 0;
        If Not Result Then
            Inc(InvalidCounter);
    Finally
        VOCrit.Leave;
    End;
End;

Procedure RemoveValidObject(Const p: Pointer);
Begin
    If Not Assigned(ValidObjects) Then
        Exit;
    VOCrit.Enter;
    Try
        If DebugHook <> 0 Then
            If Not isValidObject(p) Then
                Raise Exception.Create('Removing invalid object');
        ValidObjects.Remove(p);
    Finally
        VOCrit.Leave;
    End;
End;

Procedure InitValidObjects;
Begin
    If Assigned(ValidObjects) Then
        Exit;
    VOCrit := TCriticalSection.Create;
    ValidObjects := TList.Create;
    InvalidCounter := 0;
End;

Procedure FinalizeValidObjects;
Begin
    VOCrit.Enter;
    Try
        FreeAndNil(ValidObjects);
    Finally
        VOCrit.Leave;
    End;
    FreeAndNil(VOCrit);
End;

Function Bool2S(Const b: Boolean): String;
Begin
    If b Then
        Result := 'true'
    Else
        Result := 'false';
End;

////////////////////////////////////////////////////////////////////////////////
//
//   TPipeThread
//
////////////////////////////////////////////////////////////////////////////////
Procedure TPipeThread.SafeSynchronize(Method: TThreadMethod);
Begin
    // Exception trap
    Try
        // Call
        Synchronize(Method);
    Except
        // Trap and eat the exception, just call terminate on the thread
        Terminate;
    End;

End;

////////////////////////////////////////////////////////////////////////////////
//
//   TWriteQueue
//
////////////////////////////////////////////////////////////////////////////////
Constructor TWriteQueue.Create;
Begin
    // Perform inherited
    Inherited Create;

    InitializeCriticalSection(FCritical);

    // Starting values
    FHead := Nil;
    FTail := Nil;
    FDataEv := CreateEvent(Nil, True, False, Nil);
    AddValidObject(self);
End;

Destructor TWriteQueue.Destroy;
Begin
    If Not Assigned(self) Or Not IsValidObject(self) Then
        Exit;
    RemoveValidObject(self);
    // Clear
    Clear;

    // Close the data event handle
    CloseHandle(FDataEv);

    DeleteCriticalSection(FCritical);

    // Perform inherited
    Inherited Destroy;
End;

Procedure TWriteQueue.Clear;
Var
    node: PWriteNode;
Begin
    EnterCriticalSection(FCritical);

    Try
        // Reset the writer event
        ResetEvent(FDataEv);
        // Free all the items in the stack
        While Assigned(FHead) Do Begin
            // Get the head node and push forward
            node := FHead;
            FHead := FHead^.NextNode;
            // Free the pipe write data
            DisposePipeWrite(node^.PipeWrite);
            // Free the queued node
            FreeMem(node);
        End;

        // Set the tail to nil
        FTail := Nil;

        // Reset the count
    Finally
        LeaveCriticalSection(FCritical);
    End;
End;

Function TWriteQueue.NewNode(PipeWrite: PPipeWrite): PWriteNode;
Begin
    // Allocate memory for new node
    result := AllocMem(SizeOf(TWriteNode));

    // Set the structure fields
    result^.PipeWrite := PipeWrite;
    result^.NextNode := Nil;
End;

Procedure TWriteQueue.Enqueue(PipeWrite: PPipeWrite);
Var
    node: PWriteNode;
Begin
    // Create a new node
    node := NewNode(PipeWrite);

    EnterCriticalSection(FCritical);
    Try
        // Make this the last item in the queue
        If (FTail = Nil) Then
            FHead := node
        Else
            FTail^.NextNode := node;

        // Update the new tail
        FTail := node;

        // Set the write event to signalled
        SetEvent(FDataEv);
    Finally
        LeaveCriticalSection(FCritical);
    End;
End;

Function TWriteQueue.GetCount: Integer;
Var
    p: PWriteNode;
Begin
    Result := 0;

    EnterCriticalSection(FCritical);
    Try
        p := FHead;
        While Assigned(p) Do Begin
            Inc(Result);
            p := p^.NextNode;
        End;
    Finally
        LeaveCriticalSection(FCritical);
    End;
End;

Function TWriteQueue.Dequeue: PPipeWrite;
Var
    node: PWriteNode;
Begin
    Result := Nil;
    If Not IsValidObject(self) Then
        Exit;
    EnterCriticalSection(FCritical);
    Try
        // Remove the first item from the queue
        If Not Assigned(FHead) Then
            result := Nil
        Else Begin
            // Set the return data
            result := FHead^.PipeWrite;
            // Move to next node, update head (possibly tail), then free node
            node := FHead;
            If (FHead = FTail) Then
                FTail := Nil;
            FHead := FHead^.NextNode;
            // Free the memory for the node
            FreeMem(node);
        End;

        // Reset the write event if no more data records to write
        If GetCount = 0 Then
            ResetEvent(FDataEv);
    Finally
        LeaveCriticalSection(FCritical);
    End;
End;

////////////////////////////////////////////////////////////////////////////////
//
//   Utility functions
//
////////////////////////////////////////////////////////////////////////////////
Function AllocPipeWrite(Const Buffer; Count: Integer): PPipeWrite;
Begin

    // Allocate memory for the result
    result := AllocMem(SizeOf(TPipeWrite));

    // Set the count of the buffer
    result^.Count := Count;

    // Allocate enough memory to store the data buffer, then copy the data over
    result^.Buffer := AllocMem(Count);
    System.Move(Buffer, result^.Buffer^, Count);

End;

Procedure DisposePipeWrite(PipeWrite: PPipeWrite);
Begin
    If Assigned(PipeWrite) Then Begin
        // Dispose of the memory being used by the pipe write structure
        If Assigned(PipeWrite^.Buffer) Then FreeMem(PipeWrite^.Buffer);
        // Free the memory record
        FreeMem(PipeWrite);
    End;
End;

Procedure CheckPipeName(Value: String);
Begin
    // Validate the pipe name
    If (Pos('\', Value) > 0) Or
    (Length(Value) > 63) Or
    (Length(Value) = 0) Then Begin
        Raise EPipeException.CreateRes(PResStringRec(@resBadPipeName));
    End;

End;

Procedure InitializeSecurity(Var SA: TSecurityAttributes);
Var
    sd: PSecurityDescriptor;
Begin

    // Allocate memory for the security descriptor
    sd := AllocMem(SECURITY_DESCRIPTOR_MIN_LENGTH);

    // Initialise the new security descriptor
    InitializeSecurityDescriptor(sd, SECURITY_DESCRIPTOR_REVISION);

    // Add a NULL descriptor ACL to the security descriptor
    SetSecurityDescriptorDacl(sd, True, Nil, False);

    // Set up the security attributes structure
    With SA Do Begin
        nLength := SizeOf(TSecurityAttributes);
        lpSecurityDescriptor := sd;
        bInheritHandle := True;
    End;

End;

Procedure FinalizeSecurity(Var SA: TSecurityAttributes);
Begin

    // Release memory that was assigned to security descriptor
    If Assigned(SA.lpSecurityDescriptor) Then Begin
        FreeMem(SA.lpSecurityDescriptor);
        SA.lpSecurityDescriptor := Nil;
    End;

End;

Initialization
    InitValidObjects;
    isMainThread := true;

Finalization
    FinalizeValidObjects;

End.

