(*******************************************************************************
  Project  : AppWatcher
  Unit     : AppWatcherAgent_Stop.pas
  Author   : mbaumsti
  GitHub   : https://github.com/mbaumsti/Delphi-App-Watcher.git
  Date     : 24/02/2025
  Version  : 1.1
  License  : MIT

  Description :
  -------------
  This unit manages the **stop notification dialog** for the Agent.
  It displays a message when an application is stopping or restarting.
  **No user confirmation is required**.

  Features :
  -----------
  - Displays a **progress bar** countdown before stopping an application
  - Shows a notification when an application is **stopped or restarted**
  - Allows users to **close the dialog manually** after a stop or restart

  Change Log :
  ------------
  - 09/02/2025 : Initial creation
  - 15/02/2025 : Delete countdown timer (replace in Agent Main)
  - [22/02/2025] : Replaced the singleton AppLangManager with a local instance to allow multiple instances.
  - [23/02/2025] : v1.1 Added dynamic application title translation based on selected language
  - [24/02/2025] : v1.2 Improved configuration file lookup to support shortcut resolution.

  *******************************************************************************)

unit AppWatcherAgent_Stop;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.ComCtrls,
    Vcl.StdCtrls, IdContext, AppWatcher_Lang;

type

    TFormStopNotification = class(TForm)
        LabelMessage: TLabel;
        ProgressBar1: TProgressBar;
        BtnFermer: TButton;
        procedure FormCreate(Sender: TObject);

        procedure BtnFermerClick(Sender: TObject);
    private
        FTimeRemaining: Integer;
        FAppName:       string;

    public
        FLanguageManager: TAppLangManager;

        procedure StartCountdown(Duration: Integer; const AppName: string);
        procedure StepCountdown;
        procedure CancelStop;
        procedure NotifyStop;
        procedure NotifyRestart;
        property AppName: String read FAppName;
    end;

    //var
    //FormStopNotification: TFormStopNotification;

implementation

{$R *.dfm}


procedure TFormStopNotification.FormCreate(Sender: TObject);
var
    hSysMenu: HMENU;
begin
    BtnFermer.Visible := False;
    //Supprime le bouton de fermeture (X)
    hSysMenu := GetSystemMenu(Handle, False);
    if hSysMenu <> 0 then
        DeleteMenu(hSysMenu, SC_CLOSE, MF_BYCOMMAND);
end;

procedure TFormStopNotification.StartCountdown(Duration: Integer; const AppName: string);
begin
    BtnFermer.Visible := False;
    FAppName := AppName;
    ProgressBar1.Visible := true;
    ProgressBar1.Max := Duration;
    ProgressBar1.Position := 0;
    if assigned(FLanguageManager) then
        LabelMessage.Caption := Format(FLanguageManager.GetMessage('DLGSTOP', 'COUNTDOWN'), [FAppName, Duration]);
end;

procedure TFormStopNotification.StepCountdown;
begin
    ProgressBar1.Stepit;
    ProgressBar1.Visible := true;
    if assigned(FLanguageManager) then
    begin
        LabelMessage.Caption := Format(FLanguageManager.GetMessage('DLGSTOP', 'COUNTDOWN'), [FAppName, ProgressBar1.Max - ProgressBar1.Position]);

        if ProgressBar1.Position >= ProgressBar1.Max then
            LabelMessage.Caption := Format(FLanguageManager.GetMessage('DLGSTOP', 'STOPPING'), [FAppName]);
    end;
    application.processMessages;

end;

procedure TFormStopNotification.NotifyStop;
begin
    ProgressBar1.Position := 0;
    ProgressBar1.Visible := False;
    if assigned(FLanguageManager) then
        LabelMessage.Caption := Format(FLanguageManager.GetMessage('DLGSTOP', 'WAIT'), [FAppName]);
    BtnFermer.Visible := true;
end;

procedure TFormStopNotification.NotifyRestart;
begin
    ProgressBar1.Position := 0;
    ProgressBar1.Visible := False;
    if assigned(FLanguageManager) then
        LabelMessage.Caption := Format(FLanguageManager.GetMessage('DLGSTOP', 'RESTART'), [FAppName]);
    BtnFermer.Visible := true;
end;

procedure TFormStopNotification.BtnFermerClick(Sender: TObject);
begin
    close;
end;

procedure TFormStopNotification.CancelStop;
begin
    ProgressBar1.Position := 0;
    ProgressBar1.Visible := False;
    if assigned(FLanguageManager) then
        LabelMessage.Caption := Format(FLanguageManager.GetMessage('DLGSTOP', 'CANCEL'), [FAppName]);
    BtnFermer.Visible := true;
end;

end.
