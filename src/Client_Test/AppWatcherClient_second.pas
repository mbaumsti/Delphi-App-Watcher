 (*******************************************************************************
  Project  : AppWatcher
  Unit     : AppWatcherClient_second.pas
  Author   : mbaumsti
  GitHub   : https://github.com/mbaumsti/Delphi-App-Watcher.git
  Date     : 20/02/2025
  Version  : 1.0
  License  : MIT

  Description :
  -------------
  This unit defines a **secondary form** (`TFormAppWatcherClientSecond`) used in the
  AppWatcher test client.

  The form contains a **TAppWatcherClient component**, but during execution,
  it demonstrates that **only the client on the main form (MainForm) can interact with the Agent**.

  This allows developers to understand that :
  - A `TAppWatcherClient` instance must be **unique per application**.
  - If multiple forms contain a `TAppWatcherClient`, only the **one on the MainForm** is active.
  - Shared forms that contain `TAppWatcherClient` might behave differently
    depending on whether they are used as the **main form** or a **secondary form**.

  Features :
  -----------
  - Contains a **TAppWatcherClient** instance (inactive due to the MainForm’s priority).
  - Helps developers understand `TAppWatcherClient` lifecycle in multi-form applications.

  Change Log :
  ------------
  - [19/02/2025] : Initial creation

  Notes :
  -------
  This unit is part of the **AppWatcher test client** and is meant for testing purposes only.
  *******************************************************************************)


unit AppWatcherClient_second;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  AppWatcherClient_Component;

type
  TFormAppWatcherClient2 = class(TForm)
    Memo1: TMemo;
    AppWatcherClient1: TAppWatcherClient;
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  FormAppWatcherClient2: TFormAppWatcherClient2;

implementation

{$R *.dfm}

end.
