program DatabaseClient;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {MainDlg},
  LoginForm in 'LoginForm.pas' {LoginDlg},
  Book in 'Book.pas',
  Author in 'Author.pas',
  BookForm in 'BookForm.pas' {BookDlg},
  AuthorForm in 'AuthorForm.pas' {AuthorDlg},
  Common in 'Common.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainDlg, MainDlg);
  Application.CreateForm(TLoginDlg, LoginDlg);
  Application.CreateForm(TBookDlg, BookDlg);
  Application.CreateForm(TAuthorDlg, AuthorDlg);
  Application.Run;
end.
