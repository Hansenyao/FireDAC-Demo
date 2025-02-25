unit BookForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Common, Book, Vcl.StdCtrls;

type
  TBookDlg = class(TForm)
    ButtonSubmmit: TButton;
    ButtonCancel: TButton;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonSubmmitClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
  private
    { Private declarations }
    Book: TBook;
  public
    { Public declarations }
    Mode: TTableUpdateMode;
    function GetBook(): TBook;
  end;

var
  BookDlg: TBookDlg;

implementation

{$R *.dfm}

procedure TBookDlg.FormShow(Sender: TObject);
begin
  self.Caption := 'Book - ';
  if self.Mode = Edit then
    self.Caption := self.Caption + 'Edit'
  else  
    self.Caption := self.Caption + 'New';
end;

procedure TBookDlg.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  //
end;

procedure TBookDlg.ButtonSubmmitClick(Sender: TObject);
begin                     
  ModalResult := mrOk; 
end;
          
procedure TBookDlg.ButtonCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

function TBookDlg.GetBook(): TBook;
begin
  Result := Book;
end;

end.
