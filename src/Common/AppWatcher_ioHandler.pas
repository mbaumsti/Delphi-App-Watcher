(*******************************************************************************
  Project  : AppWatcher
  Unit     : AppWatcher_ioHandler.pas
  Author   : mbaumsti
  GitHub   : https://github.com/mbaumsti/Delphi-App-Watcher.git
  Date     : 24/02/2025
  Version  : 2.0.0
  License  : MIT

  Description :
  -------------
  This unit handles **message transmission and reception** between AppWatcher components.
  It defines the message structure and processes data transfer over **Indy TCP** (`IdIOHandler`).

  Features :
  -----------
  - Defines **commands (STOP, WHO, START, etc.)**
  - Handles **sending and receiving** messages over TCP
  - Ensures correct **message formatting**
  - Implements **buffer validation** before reading messages

  Change Log :
  ------------
  - [09/02/2025] : Initial creation
  - [12/02/2025] : Added support for `STOP_REQUEST` and `REPLY_STOP_REQUEST` commands
  - [23/02/2025] : v1.1 Added dynamic application title translation based on selected language
  - [24/02/2025] : v1.2 Improved configuration file lookup to support shortcut resolution.
  - [06/03/2025] : v2.0 - Improved 32/64 bit compatibility:
                          * Handle changed from HWnd to UINT64.
                          * Added packed on TAppWatcherMessage
                          !!! This change makes the version incompatible with v1 !!!
                        - Minor modification : Replaced TAppWatcherStrCommand with GetEnumName(TypeInfo(TAppWatcherCommand)
                        - Minor modification : Added GetCmdName function


*******************************************************************************)

Unit AppWatcher_ioHandler;

Interface

Uses windows, System.Classes, System.SysUtils, System.TypInfo, IdTCPClient, IdTCPConnection, IdComponent, IdGlobal,
    IdBaseComponent, IdIOHandler, dialogs;

Type
    TAppWatcherCommand = (cmdUNKNOWN, cmdACK, cmdNACK, cmdSTOP, cmdWHO, cmdWHO_REPLY, cmdSTART, cmdCANCEL, cmdSTOP_REQUEST, cmdREPLY_STOP_REQUEST, cmdSTOP_AGENT);

    TAppWatcherMessage = Packed Record
        //  TAppWatcherMessage = record
        Command: TAppWatcherCommand; // Enum
        UserName: String[255]; //Nom de l'utilisateur
        Handle: UInt64; // 🔹 Taille fixe pour compatibilité 32/64 bits au lieu de HWnd;
        //    Handle : HWnd;
        AppName: String[255]; // Nom
        AppPath: String[255]; // Chemin complet
        Params: String[255]; // Arguments
        Duration: Integer; // Temps avant arrêt
        Procedure Clear;
        Procedure Init(AHandle: HWnd; ACommand: TAppWatcherCommand; AAppPath, AParams: String; ADuration: integer);
        Procedure SendMessage(AIOHandler: TIdIOHandler);
        Function CmdName: String;
    End;

Function ReadMessage(AIOHandler: TIdIOHandler; Out Msg: TAppWatcherMessage): Boolean;
Function GetCmdName(ACmd: TAppWatcherCommand): String;

Implementation

//Renvoi le nom de l'utilisateur courant
Function CurUSer: String;
Var
    u: Array[0..127] Of Char;
    sz: DWORD;
Begin
    sz := sizeof(u);
    GetUserName(u, sz);
    Result := uppercase(u);
End;

Function GetCmdName(ACmd: TAppWatcherCommand): String;
Begin
    Result := Copy(GetEnumName(TypeInfo(TAppWatcherCommand), Ord(ACmd)), Length('cmd') + 1);
End;

Procedure TAppWatcherMessage.Clear;
Begin
    Self := Default(TAppWatcherMessage);
End;

Function TAppWatcherMessage.CmdName: String;
Begin
    Result := GetCmdName(Command);
End;

Procedure TAppWatcherMessage.Init(AHandle: HWnd; ACommand: TAppWatcherCommand; AAppPath, AParams: String; ADuration: integer);
Begin
    Handle := AHandle;
    Command := ACommand;
    AppPath := AAppPath;
    AppName := ExtractFileName(AppPath);
    Params := AParams;
    Duration := ADuration;
    UserName := CurUser;
End;

Procedure TAppWatcherMessage.SendMessage(AIOHandler: TIdIOHandler);
Var
    MsgSize: Int32;
    Buffer: TIdBytes;
Begin
    If Not Assigned(AIOHandler) Then Exit;

    // Calcul de la taille du message
    MsgSize := SizeOf(Self);

    // Préparer le buffer
    SetLength(Buffer, SizeOf(Int32) + MsgSize);

    // Copier la taille au début
    Move(MsgSize, Buffer[0], SizeOf(Int32));

    // Copier le message juste après
    Move(Self, Buffer[SizeOf(Int32)], MsgSize);

    // Envoyer le buffer complet
    AIOHandler.Write(Buffer, SizeOf(Int32) + MsgSize);
    AIOHandler.WriteBufferFlush;
End;

Function ReadMessage(AIOHandler: TIdIOHandler; Out Msg: TAppWatcherMessage): Boolean;
Var
    MsgSize: Int32;
    Buffer: TIdBytes;
    i: Integer;
    ByteVal: Byte;
Begin
    Result := False;
    Msg.Clear;

    If Not Assigned(AIOHandler) Or Not AIOHandler.Connected Then Exit;

    Try
        // 🔍 Vérifier qu’au moins 4 octets sont disponibles pour la taille
        If AIOHandler.InputBuffer.Size < SizeOf(Int32) Then Exit;

        // 🔎 Lire chaque octet de la taille avec PeekByte (sans consommer)
        MsgSize := 0;
        For i := 0 To 3 Do Begin
            ByteVal := AIOHandler.InputBuffer.PeekByte(i);
            MsgSize := MsgSize Or (ByteVal Shl (i * 8)); // Reconstruction de l'Int32
        End;

        // 🚨 Vérifier que la taille est correcte
        If MsgSize <> SizeOf(TAppWatcherMessage) Then Exit;

        // 🛑 Vérifier que le message complet est disponible avant de lire
        If AIOHandler.InputBuffer.Size < (SizeOf(Int32) + MsgSize) Then Exit;

        // 🔹 Maintenant, consommer la taille du message
        AIOHandler.ReadBytes(Buffer, SizeOf(Int32), False);

        // 🔹 Lire le message entier
        AIOHandler.ReadBytes(Buffer, MsgSize, False);
        Move(Buffer[0], Msg, MsgSize);
        //        MessageDlg(paramstr(0)+' - Duration='+Msg.Duration.ToString,mtInformation,[mbok],0);

        Result := True; // ✅ Message bien reçu et décodé
    Except
        On E: Exception Do Begin
            // 🚨 Gestion des erreurs réseau
            Exit;
        End;
    End;
End;

End.

