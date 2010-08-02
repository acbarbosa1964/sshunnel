unit SSHunnel;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ShellAPI,
  Menus, StdCtrls, ExtCtrls;

const
  WM_ICONTRAY = WM_USER + 1;

type
  TForm6 = class(TForm)
    PopupMenu1: TPopupMenu;
    pcon: TMenuItem;
    pdiscon: TMenuItem;
    pExit: TMenuItem;
    psettings: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    GroupBox1: TGroupBox;
    Edit1: TEdit;
    RadioGroup1: TRadioGroup;
    Edit2: TEdit;
    Edit3: TEdit;
    GroupBox2: TGroupBox;
    Edit4: TEdit;
    RadioGroup2: TRadioGroup;
    Edit5: TEdit;
    Label1: TLabel;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pExitClick(Sender: TObject);
    procedure psettingsClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure pconClick(Sender: TObject);
    procedure pdisconClick(Sender: TObject);
  private
    { Private declarations }
  public
    procedure TrayMessage(var Msg: TMessage); message WM_ICONTRAY;
    { Public declarations }
  end;

var
  Form6: TForm6;
  NotifyIconData : TNotifyIconData;

implementation

{$R *.dfm}

procedure TForm6.Button1Click(Sender: TObject);
begin
  Self.Hide;
end;

procedure TForm6.FormCreate(Sender: TObject);
begin
  with NotifyIconData do begin
    hIcon := Application.Icon.Handle;
    StrPCopy(szTip, Application.Title);
    Wnd := Handle;
    uCallbackMessage := WM_ICONTRAY;
    uID := 1;
    uFlags := NIF_MESSAGE + NIF_ICON + NIF_TIP;
    cbSize := sizeof(TNotifyIconData);
  end;
  Shell_NotifyIcon(NIM_ADD, @NotifyIconData);
end;

procedure TForm6.FormDestroy(Sender: TObject);
begin
  Shell_NotifyIcon(NIM_DELETE, @NotifyIconData);
end;

procedure TForm6.pconClick(Sender: TObject);
begin
  pcon.Enabled:=False;
  //Connect

  pdiscon.Enabled:=True;
end;

procedure TForm6.pdisconClick(Sender: TObject);
begin
  pcon.Enabled:=True;
  pdiscon.Enabled:=False;
end;

procedure TForm6.pExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TForm6.psettingsClick(Sender: TObject);
begin
  Self.Show;
end;


procedure TForm6.TrayMessage(var Msg: TMessage);
var
p : TPoint;
begin
  case Msg.lParam of
    WM_LBUTTONDOWN:
    begin
    //Nothing
    end;
    WM_RBUTTONDOWN:
    begin
    //Right Pressed
    SetForegroundWindow(Handle);
    GetCursorPos(p);
    PopUpMenu1.Popup(p.x, p.y);
    PostMessage(Handle, WM_NULL, 0, 0);
    end;
  end;
end;


end.
