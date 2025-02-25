unit AuthorForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Common, Author, Vcl.StdCtrls;

type
  TAuthorDlg = class(TForm)
    ButtonSubmmit: TButton;
    ButtonCancel: TButton;
    Label1: TLabel;
    EditFirstName: TEdit;
    Label2: TLabel;
    EditLastName: TEdit;
    Label3: TLabel;
    EditCountry: TEdit;
    Label4: TLabel;
    EditCity: TEdit;
    Label5: TLabel;
    EditContactUrl: TEdit;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure ButtonSubmmitClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    Author: TAuthor;
  public
    { Public declarations }
    Mode: TTableUpdateMode;
    procedure SetAuthor(Author: TAuthor);
    function GetAuthor(): TAuthor;
  end;

var
  AuthorDlg: TAuthorDlg;

implementation

{$R *.dfm}

procedure TAuthorDlg.FormShow(Sender: TObject);
begin
  // Set caption text and default data based on mode
  self.Caption := 'Author - ';
  if self.Mode = Edit then
    begin
      self.Caption := self.Caption + 'Edit';
      EditFirstName.Text := Author.FirstName;
      EditLastName.Text := Author.LastName;
      EditCountry.Text := Author.ResidentCountry;
      EditCity.Text := Author.ResidentCity;
      EditContactUrl.Text := Author.ContactUrl;
    end
  else
    self.Caption := self.Caption + 'New';

  // Set focus on the first component
  EditFirstName.SetFocus();
end;

procedure TAuthorDlg.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  //
end;

procedure TAuthorDlg.FormDestroy(Sender: TObject);
begin
  //
end;

procedure TAuthorDlg.ButtonSubmmitClick(Sender: TObject);
var
  FirstName: string;
  LastName: string;
  Country: string;
  City: string;
  ContactUrl: string;
begin
  // Validate inputtings
  FirstName := EditFirstName.Text;
  if FirstName.IsEmpty then
    begin
      ShowMessage('First Name must be provided.');
      EditFirstName.SetFocus();
      Exit;
    end;
  LastName := EditLastName.Text;
  if LastName.IsEmpty then
    begin
      ShowMessage('Last Name must be provided.');
      EditLastName.SetFocus();
      Exit;
    end;
  ContactUrl := EditContactUrl.Text;
  if ContactUrl.IsEmpty then
    begin
      ShowMessage('Contact Url must be provided.');
      EditContactUrl.SetFocus();
      Exit;
    end;
  Country := EditCountry.Text;
  City := EditCity.Text;

  // Update data to data member
  Author := TAuthor.Create(FirstName, LastName, ContactUrl, City, Country);

  // Return OK
  ModalResult := mrOk;
end;

procedure TAuthorDlg.ButtonCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TAuthorDlg.SetAuthor(Author: TAuthor);
begin
  self.Author := Author;
end;

function TAuthorDlg.GetAuthor(): TAuthor;
begin
  Result := Author;
end;

end.
