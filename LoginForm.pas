unit LoginForm;

{
  Class: TLoginDlg
  Introduce: The dialog for users to login, asking users to input User Name and Password
  Date: 24/02/2025
}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TLoginDlg = class(TForm)
    ButtonOK: TButton;
    LabelUserName: TLabel;
    EditUserName: TEdit;
    LabelPassword: TLabel;
    EditPassword: TEdit;
    ButtonCancel: TButton;
    procedure ButtonOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
  private
    { Private declarations }
    UserName: string;
    Password: string;
  public
    { Public declarations }
    function GetUserName(): string;
    function GetPassword(): string;
  end;

var
  LoginDlg: TLoginDlg;

implementation

{$R *.dfm}

{Initialize UserName and Passowrd when this window shows}
procedure TLoginDlg.FormShow(Sender: TObject);
begin
  UserName := '';
  Password := '';
  EditUserName.SetFocus;
end;

{Try to login}
procedure TLoginDlg.ButtonOKClick(Sender: TObject);
begin
  // User Name cannot be empty
  UserName := EditUserName.Text;
  if UserName.IsEmpty then
    begin
      ShowMessage('Please input User Name.');
      EditUserName.SetFocus;
      Exit;
    end;

  // Password cannot be empty
  Password := EditPassword.Text;
  if Password.IsEmpty then
    begin
      ShowMessage('Please input Password.');
      EditPassword.SetFocus;
      Exit;
    end;

  // Return mrOk
  ModalResult := mrOk;
end;

{Canceled by user}
procedure TLoginDlg.ButtonCancelClick(Sender: TObject);
begin
  // Return mrCancel
  ModalResult := mrCancel;
end;

{Return the User Name string}
function TLoginDlg.GetUserName: string;
begin
  Result := UserName;
end;

{Return the Password string}
function TLoginDlg.GetPassword: string;
begin
  Result := Password;
end;

end.
