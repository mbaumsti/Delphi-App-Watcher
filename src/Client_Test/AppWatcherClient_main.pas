(*******************************************************************************
  Project  : AppWatcher
  Unit     : AppWatcherClient_main.pas
  Author   : mbaumsti
  GitHub   : https://github.com/mbaumsti/Delphi-App-Watcher.git
  Date     : 09/04/2025
  Version  : 3.0.0
  License  : MIT

  Description :
  -------------
  This is a **test client** for the AppWatcher project.
  It connects to the **AppWatcher Master** using the `TAppWatcherClient` component
  and listens for commands such as **STOP, WHO, and START**.

  Features :
  -----------
  - Displays received commands in a **memo log**
  - Responds to **STOP** requests
  - Provides application parameters when requested
  - Useful for debugging communication with the Master

  Change Log :
  ------------
  - [09/02/2025] : Initial creation
  - [15/02/2025] : Added support for `OnStopRequested` event
  - [19/02/2025] : Improved logging of received commands
  - [22/02/2025] : Adding StopRequested
  - [22/02/2025] : Replaced singleton `AppLangManager` with a local instance, allowing multiple instances of `TAppWatcherClient` to have different languages.
  - [23/02/2025] : v1.1 Added dynamic application title translation based on selected language
  - [24/02/2025] : v1.2 Improved configuration file lookup to support shortcut resolution.
  - [06/03/2025] : v2.0 - Use of New  TAppWatcherMessage with Packed Record
                         !!! This change makes the version incompatible with v1 !!!
                        - Improvement: Use of getCmdName
                        - Improvement: Added radio buttons to simulate accepting or refusing to close
                        - Improvement: Added parameters to show how transmission works on close/start
  - [09/04/2025] : v3.0- Adapted to use named pipes instead of TCPIP for communication with the local agent.

  Notes :
  -------
  This unit is a **test tool** and should not be used in production.
  Contributions and improvements are welcome!
  *******************************************************************************)

Unit AppWatcherClient_main;

Interface

Uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, IdBaseComponent, IdComponent, System.TypInfo,
    IdTCPConnection, IdTCPClient, Vcl.ExtCtrls, Vcl.StdCtrls,
    AppWatcherClient_Component, AppWatcher_ioHandler,
    AppWatcherClient_second, AppWatcher_Lang, System.IOUtils, Winapi.ShlObj, ActiveX, ComObj;

Type

    TFormAppWatcherClient = Class(TForm)
        Memo1: TMemo;
        Button1: TButton;
        AppWatcherClient1: TAppWatcherClient;
        RdioStopAccepted: TRadioButton;
        rdioStopRefused: TRadioButton;

        Procedure AppWatcherClient1CommandReceived(Sender: TObject; Const Command:
            TAppWatcherCommand);
        Procedure AppWatcherClient1GetAppParams(Sender: TObject; Var Params: String);
        Procedure AppWatcherClient1StopRequested(Sender: TObject;
            Var CanStop: Boolean);
        Procedure Button1Click(Sender: TObject);
        Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
        Procedure FormCreate(Sender: TObject);

    private
        {Déclarations privées}

    public
        {Déclarations publiques}
    End;

Var
    FormAppWatcherClient: TFormAppWatcherClient;

Implementation

{$R *.dfm}

Procedure TFormAppWatcherClient.AppWatcherClient1CommandReceived(Sender:
    TObject; Const Command: TAppWatcherCommand);
Begin
    Memo1.lines.add('Received in  AppWatcherClient1CommandReceived :' + GetCmdName(Command));
End;

Procedure TFormAppWatcherClient.AppWatcherClient1GetAppParams(Sender: TObject;
    Var Params: String);
Begin
    // Ici il est possible de transmettre à AppWatcher les paramètres nécessaires au redémarrage de l'application
    // Here it is possible to transmit to AppWatcher the parameters necessary for restarting the application
    If RdioStopAccepted.Checked Then
        Params := ' -Stop:Yes '
    Else
        Params := ' -Stop:No ';
    Memo1.lines.add('Params=' + Params);

End;

Procedure TFormAppWatcherClient.AppWatcherClient1StopRequested(
    Sender: TObject; Var CanStop: Boolean);
Begin
    //AppWatcher demande à l'application si elle accepte la fermeture
    //AppWatcher asks the application if it accepts the closing
    If RdioStopAccepted.Checked Then
        CanStop := true
    Else
        CanStop := False;
End;

Procedure TFormAppWatcherClient.Button1Click(Sender: TObject);
Begin
    FormAppWatcherClientSecond.showModal;
End;

Procedure TFormAppWatcherClient.FormCloseQuery(Sender: TObject; Var CanClose:
    Boolean);
Begin
    // On peut empecher la fermeture de la form par l'utilisateur mais l'approuver si c'est AppWatcher qui le demande
    // We can prevent the user from closing the form but approve it if AppWatcher requests it.
    CanClose := RdioStopAccepted.Checked;

    If AppWatcherClient1.CloseRequested Then
        CanClose := true;
End;

Procedure TFormAppWatcherClient.FormCreate(Sender: TObject);
Var
    StopVal: String;
Begin
    //🔹 Récupération des paramètres de la ligne de commande
    Memo1.Lines.add('Start with params =' + CmdLine);
    //🔹 Retrieving command line parameters
    If FindCmdLineSwitch('Stop', StopVal, True, [clstValueAppended]) Then Begin
        StopVal := LowerCase(StopVal);
        If (StopVal = 'yes') Or (StopVal = 'y') Or (StopVal = 'oui') Or (StopVal = 'o') Then
            RdioStopAccepted.Checked := true
        Else
            RdioStoprefused.Checked := true;
    End;
End;

End.

