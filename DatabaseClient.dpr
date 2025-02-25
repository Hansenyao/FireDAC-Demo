program DatabaseClient;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {MainDlg},
  LoginForm in 'LoginForm.pas' {LoginDlg},
  Book in 'Book.pas',
  Author in 'Author.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainDlg, MainDlg);
  Application.CreateForm(TLoginDlg, LoginDlg);
  Application.Run;
end.
