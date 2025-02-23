(*******************************************************************************
  Project  : AppWatcher
  Unit     : AppWatcher_ioHandler.pas
  Author   : mbaumsti
  GitHub   : https://github.com/mbaumsti/Delphi-App-Watcher.git
  Date     : 24/02/2025
  Version  : 1.2
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

*******************************************************************************)

unit AppWatcher_ioHandler;

interface

uses windows,  System.Classes, System.SysUtils,IdTCPClient, IdTCPConnection, IdComponent, IdGlobal,
    IdBaseComponent,IdIOHandler,dialogs;

type
  TAppWatcherCommand = (cmdUnknown, cmdACK, cmdNACK, cmdSTOP, cmdWHO,cmdWHO_REPLY, cmdSTART, cmdCANCEL,cmdSTOP_REQUEST,cmdREPLY_STOP_REQUEST,cmdSTOP_AGENT);

  TAppWatcherMessage = record
    Command: TAppWatcherCommand; // Enum
    UserName : String[255]; //Nom de l'utilisateur
    Handle : HWnd;
    AppName: string[255];  // Nom
    AppPath: string[255];  // Chemin complet
    Params: string[255];   // Arguments
    Duration: Integer;     // Temps avant arrêt
    procedure Clear;
    procedure Init(AHandle: HWnd; ACommand :TAppWatcherCommand;AAppPath ,AParams : string; ADuration : integer  );
    procedure SendMessage(AIOHandler: TIdIOHandler);
    Function CmdName : string;
  end;

  function ReadMessage(AIOHandler: TIdIOHandler; out Msg: TAppWatcherMessage): Boolean;

const
  TAppWatcherStrCommand: array[0..10] of string =
    ('UNKNOWN', 'ACK', 'NACK', 'STOP', 'WHO','WHO_REPLY', 'START', 'CANCEL', 'STOP_REQUEST', 'REPLY_STOP_REQUEST','STOP_AGENT');

implementation

//Renvoi le nom de l'utilisateur courant
function CurUSer: string;
var
    u:  array [0 .. 127] of Char;
    sz: DWORD;
begin
    sz := sizeof(u);
    GetUserName(u, sz);
    Result := uppercase(u);
end;

procedure TAppWatcherMessage.Clear;
begin
  Self := Default(TAppWatcherMessage);
end;

Function TAppWatcherMessage.CmdName : string;
begin
   Result:=  TAppWatcherStrCommand[ord(Command)];
end;

procedure TAppWatcherMessage.Init(AHandle: HWnd;ACommand :TAppWatcherCommand;AAppPath ,AParams : string; ADuration : integer  );
Begin
  Handle := AHandle;
  Command:=ACommand;
  AppPath:=AAppPath;
  AppName:= ExtractFileName(AppPath );
  Params:=AParams;
  Duration:=ADuration;
  UserName:=CurUser;
End;

procedure TAppWatcherMessage.SendMessage(AIOHandler: TIdIOHandler);
var
    MsgSize: Int32;
    Buffer: TIdBytes;
begin
    if not Assigned(AIOHandler) then Exit;

//    MessageDlg(paramstr(0)+' - Duration='+Duration.ToString,mtInformation,[mbok],0);

    // Calcul de la taille du message
    MsgSize := SizeOf(Self);

    // Préparer le buffer
    SetLength(Buffer, SizeOf(Int32) + MsgSize);

    // Copier la taille au début
    Move(MsgSize, Buffer[0], SizeOf(Int32));

    // Copier le message juste après
    Move(Self, Buffer[SizeOf(Int32)], MsgSize);

    // Envoyer le buffer complet
    AIOHandler.Write(Buffer,SizeOf(Int32) + MsgSize);
    AIOHandler.WriteBufferFlush;
end;

function ReadMessage(AIOHandler: TIdIOHandler; out Msg: TAppWatcherMessage): Boolean;
var
    MsgSize: Int32;
    Buffer:  TIdBytes;
    i:       Integer;
    ByteVal: Byte;
begin
    Result := False;
    Msg.Clear;

    if not Assigned(AIOHandler) or not AIOHandler.Connected then Exit;

    try
        // 🔍 Vérifier qu’au moins 4 octets sont disponibles pour la taille
        if AIOHandler.InputBuffer.Size < SizeOf(Int32) then Exit;

        // 🔎 Lire chaque octet de la taille avec PeekByte (sans consommer)
        MsgSize := 0;
        for i := 0 to 3 do
        begin
            ByteVal := AIOHandler.InputBuffer.PeekByte(i);
            MsgSize := MsgSize or (ByteVal shl (i * 8));  // Reconstruction de l'Int32
        end;

        // 🚨 Vérifier que la taille est correcte
        if MsgSize <> SizeOf(TAppWatcherMessage) then Exit;

        // 🛑 Vérifier que le message complet est disponible avant de lire
        if AIOHandler.InputBuffer.Size < (SizeOf(Int32) + MsgSize) then Exit;

        // 🔹 Maintenant, consommer la taille du message
        AIOHandler.ReadBytes(Buffer, SizeOf(Int32), False);

        // 🔹 Lire le message entier
        AIOHandler.ReadBytes(Buffer, MsgSize, False);
        Move(Buffer[0], Msg, MsgSize);
//        MessageDlg(paramstr(0)+' - Duration='+Msg.Duration.ToString,mtInformation,[mbok],0);

        Result := True;  // ✅ Message bien reçu et décodé
    except
        on E: Exception do
        begin
            // 🚨 Gestion des erreurs réseau
            Exit;
        end;
    end;
end;




end.
