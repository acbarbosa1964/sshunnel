program shunnelproj;

uses
  Forms,
  SSHunnel in 'SSHunnel.pas' {Form6};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.ShowMainForm := False;
  Application.Title := 'SSHunnel';
  Application.CreateForm(TForm6, Form6);
  Application.Run;
end.
