unit MainForm;

{
  Class: TMainDlg
  Introduce: The main dialog of this application
             Try to connect database and show table records
  Date: 24/02/2025
}

interface

uses
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error,
  FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool,
  FireDAC.Stan.Async, FireDAC.Phys, FireDAC.VCLUI.Wait, FireDAC.Stan.Param,
  FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.Phys.MSSQLDef, Data.DB,
  FireDAC.Phys.ODBCBase, FireDAC.Phys.MSSQL, Vcl.StdCtrls, Vcl.Controls,
  Vcl.Grids, Vcl.DBGrids, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Forms, Vcl.Dialogs,
  Common, Author, Book, LoginForm, BookForm, AuthorForm;

type
  TMainDlg = class(TForm)
    FDConnection: TFDConnection;
    FDQuery: TFDQuery;
    DBGridTableData: TDBGrid;
    LabelDriverID: TLabel;
    EditDriverID: TEdit;
    LabelServer: TLabel;
    EditServer: TEdit;
    LabelDatabase: TLabel;
    EditDatabase: TEdit;
    LabelOSAuthent: TLabel;
    ComboBoxOSAuthent: TComboBox;
    ButtonConnect: TButton;
    FDPhysMSSQLDriverLink: TFDPhysMSSQLDriverLink;
    LabelSelectTable: TLabel;
    ComboBoxSelectedTableName: TComboBox;
    DataSource: TDataSource;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    ButtonEditData: TButton;
    ButtonAddData: TButton;
    procedure ButtonConnectClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ComboBoxSelectedTableNameChange(Sender: TObject);
    procedure ComboBoxOSAuthentChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonEditDataClick(Sender: TObject);
    procedure ButtonAddDataClick(Sender: TObject);
  private
    { Private declarations }
    OSAuthent: Boolean;
    procedure ListAllTableNames();
    procedure SetUpdateButtonsStatus(SelectedTableName: string);
    procedure ShowAuthorsUpdateDlg(Mode: TTableUpdateMode);
    procedure ShowBooksUpdateDlg(Mode: TTableUpdateMode);
    procedure UpdateDataToAuthorsTable(AuthorId: Integer; Author: TAuthor);
    procedure AddDataToAuthorsTable(Author: TAuthor);
  public
    { Public declarations }
  end;

var
  MainDlg: TMainDlg;
  const UpdateSupportedTables: array of string = ['Authors', 'Books'];

implementation

{$R *.dfm}

{Initialize the main form}
procedure TMainDlg.FormShow(Sender: TObject);
begin
  ComboBoxOSAuthent.Items.Clear;
  ComboBoxOSAuthent.Items.Add('Yes');
  ComboBoxOSAuthent.Items.Add('No');
  ComboBoxOSAuthent.items.Objects[0] := TObject(true);
  ComboBoxOSAuthent.items.Objects[1] := TObject(false);
  ComboBoxOSAuthent.ItemIndex := 0;
  OSAuthent := true;

  DBGridTableData.DataSource := DataSource;

  EditDriverID.SetFocus();
  ButtonEditData.Enabled := false;
  ButtonAddData.Enabled := false;
end;

{Free resource when app exits}
procedure TMainDlg.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FDQuery.Close;
  FDConnection.Close;
end;

{Get the selected Authent type}
procedure TMainDlg.ComboBoxOSAuthentChange(Sender: TObject);
begin
  OSAuthent := Boolean(ComboBoxOSAuthent.items.Objects[ComboBoxOSAuthent.ItemIndex]);
end;

{To list all records in the selected table}
procedure TMainDlg.ButtonConnectClick(Sender: TObject);
var
  UserName: string;
  Password: string;
  StrText: string;
begin
  // Ask user to input UserName and Password if OSAuthent=false
  if not OSAuthent then
    begin
      var LoginDlg: TLoginDlg;
      LoginDlg := TLoginDlg.Create(Self);
      LoginDlg.Position := poOwnerFormCenter;
      if LoginDlg.ShowModal() = mrOk then
        begin
          UserName := LoginDlg.GetUserName();
          Password := LoginDlg.GetPassword();
          LoginDlg.Free;
        end
      else
        begin
          LoginDlg.Free;
          Exit;
        end;
    end;

  // Clear old connection parameters
  FDConnection.Params.Clear;
  //
  StrText := Format('DriverID=%s', [EditDriverID.Text]);
  FDConnection.Params.Add(StrText);
  //
  StrText := Format('Server=%s', [EditServer.Text]);
  FDConnection.Params.Add(StrText);
  //
  StrText := Format('Database=%s', [EditDatabase.Text]);
  FDConnection.Params.Add(StrText);
  //
  FDConnection.Params.Add('Trusted_Connection=yes');
  FDConnection.Params.Add('TrustServerCertificate=yes');
  FDConnection.Params.Add('MultipleActiveResultSets=yes');
  FDConnection.Params.Add('encrypt=No');
  //
  if OSAuthent then
    begin
      FDConnection.Params.Add('OSAuthent=yes');  // Use windows authentication
      FDConnection.LoginPrompt := false;
    end
  else
    begin
      StrText := Format('User_Name=%s', [UserName]);
      FDConnection.Params.Add(StrText);
      //
      StrText := Format('Password=%s', [Password]);
      FDConnection.Params.Add(StrText);
      //
      FDConnection.Params.Add('OSAuthent=no');  // Don't use windows authentication
      FDConnection.LoginPrompt := true;
    end;

  try
    // Try to connect the database
    FDConnection.Connected := True;

    // List all user table names in list
    ListAllTableNames();

    ShowMessage('Connect successfully!');
  except
    on E: Exception do
      ShowMessage('Connect failed: ' + E.Message);
  end;

end;

{
  Current selected table is changed,
  1. Open the table and list records
  2. Update buttons status
}
procedure TMainDlg.ComboBoxSelectedTableNameChange(Sender: TObject);
var
  TableName: string;
begin
  // close previous query
  FDQuery.Close();

  // select all records in the selected table
  TableName := ComboBoxSelectedTableName.Text;
  FDQuery.Connection := FDConnection;
  FDQuery.SQL.Text := Format('SELECT * FROM "%s"', [TableName]);

  try
    // Open this table
    FDQuery.Open();

    // Set table update buttons status
    SetUpdateButtonsStatus(TableName);
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
end;

{Show the dialog for record edit operation}
procedure TMainDlg.ButtonEditDataClick(Sender: TObject);
var
  SelectTable: string;
begin
  SelectTable := ComboBoxSelectedTableName.Text;
  if SelectTable = 'Authors' then
    begin
      ShowAuthorsUpdateDlg(Edit);
    end
  else if SelectTable = 'Books' then
    begin
      ShowBooksUpdateDlg(Edit);
    end
  else
    begin
      ShowMessage('Not implement yet.');
    end;
end;

{Show the dialog for record add operation}
procedure TMainDlg.ButtonAddDataClick(Sender: TObject);
var
  SelectTable: string;
begin
  SelectTable := ComboBoxSelectedTableName.Text;
  if SelectTable = 'Authors' then
    begin
      ShowAuthorsUpdateDlg(Add);
    end
  else if SelectTable = 'Books' then
    begin
      ShowBooksUpdateDlg(Add);
    end
  else
    begin
      ShowMessage('Not implement yet.');
    end;
end;

{List all table names in the combobox}
procedure TMainDlg.ListAllTableNames;
var
  TableList: TStringList;
  TableName: string;
  i: Integer;
begin
  TableList := TStringList.Create;
  try
    // Get all user table names and store in TableList
    // The string format is 'database.scheme.table'
    FDConnection.GetTableNames('', '', '', TableList, [osMy]);

    // Fill table name to combobo
    ComboBoxSelectedTableName.Clear();
    for i := 0 to TableList.Count - 1 do
    begin
      var StringList: TStringList;
      StringList := TStringList.Create;
      ExtractStrings(['.'], ['"'], PChar(TableList[i]), StringList);
      TableName := StringList[2];
      ComboBoxSelectedTableName.Items.Add(TableName);
      StringList.Free;
    end;
    ComboBoxSelectedTableName.ItemIndex := 0;
    self.ComboBoxSelectedTableNameChange(self.ComboBoxSelectedTableName);
  finally
    TableList.Free;
  end;
end;

{Update tables operations buttons status}
procedure TMainDlg.SetUpdateButtonsStatus(SelectedTableName: string);
var
  IsSupportUpdate : boolean;
  i: Integer;
begin
  IsSupportUpdate := false;
  for i := 0 to Length(UpdateSupportedTables) - 1 do
    begin
      if SelectedTableName = UpdateSupportedTables[i] then
        begin
          IsSupportUpdate := true;
          break;
        end;
    end;

  if IsSupportUpdate then
    begin
      ButtonEditData.Enabled := true;
      ButtonAddData.Enabled := true;
    end
  else
    begin
      ButtonEditData.Enabled := false;
      ButtonAddData.Enabled := false;
    end;
end;

{Show dialog for Authors table operation}
procedure TMainDlg.ShowAuthorsUpdateDlg(Mode: TTableUpdateMode);
var
  SelectedIndex: Integer;
  AuthorId: Integer;
  Author: TAuthor;
  AuthorDlg: TAuthorDlg;
begin
  // Get current selected record in Edit mode
  if Mode = Edit then
    begin
      SelectedIndex := DBGridTableData.DataSource.DataSet.RecNo;
      FDQuery.RecNo := SelectedIndex;
      AuthorId := FDQuery.FieldByName('AuthorId').AsInteger;
      Author := TAuthor.Create(FDQuery.FieldByName('FirstName').AsString,
                               FDQuery.FieldByName('LastName').AsString,
                               FDQuery.FieldByName('ResidentCountry').AsString,
                               FDQuery.FieldByName('ResidentCity').AsString,
                               FDQuery.FieldByName('ContactUrl').AsString);
    end;

  // Show Author Edit/Add dialog
  AuthorDlg := TAuthorDlg.Create(self);
  AuthorDlg.Position := poOwnerFormCenter;
  AuthorDlg.Mode := Mode;
  AuthorDlg.SetAuthor(Author);
  if AuthorDlg.ShowModal() = mrOk then
    begin
      // update this record to database
      Author := AuthorDlg.GetAuthor();
      if Mode = Edit then
        UpdateDataToAuthorsTable(AuthorId, Author)
      else
        AddDataToAuthorsTable(Author);
    end;
  Author.Free();
  AuthorDlg.Free();
end;

{Show dialog for Books table operation}
procedure TMainDlg.ShowBooksUpdateDlg(Mode: TTableUpdateMode);
var
  BookDlg: TBookDlg;
begin
  BookDlg := TBookDlg.Create(self);
  BookDlg.Position := poOwnerFormCenter;
  BookDlg.Mode := Mode;
  if BookDlg.ShowModal() = mrOk then
    begin

    end;
  BookDlg.Free();
end;

{Edit a reocrd in table Authors}
procedure TMainDlg.UpdateDataToAuthorsTable(AuthorId: Integer; Author: TAuthor);
begin
  try
    FDQuery.SQL.Text := 'UPDATE Authors SET ' +
                        'FirstName = :FirstName, ' +
                        'LastName = :LastName, ' +
                        'ContactUrl = :ContactUrl, ' +
                        'ResidentCountry = :ResidentCountry, ' +
                        'ResidentCity = :ResidentCity ' +
                        'WHERE AuthorId = :AuthorId';
    FDQuery.ParamByName('FirstName').AsString := Author.FirstName;
    FDQuery.ParamByName('LastName').AsString := Author.LastName;
    FDQuery.ParamByName('ContactUrl').AsString := Author.ContactUrl;
    FDQuery.ParamByName('ResidentCountry').AsString := Author.ResidentCountry;
    FDQuery.ParamByName('ResidentCity').AsString := Author.ResidentCity;
    FDQuery.ParamByName('AuthorId').AsInteger := AuthorId;

    // Submmit update to database
    FDQuery.ExecSQL;
    FDConnection.Commit;

    ShowMessage('Record updated successfully.');
  except
    on E: Exception do
    begin
      FDConnection.Rollback;
      ShowMessage('Record updated failed: ' + E.Message);
    end;
  end;

  // Refresh DBGrid
  ComboBoxSelectedTableNameChange(self);
end;

{Add a new reocrd into table Authors}
procedure TMainDlg.AddDataToAuthorsTable(Author: TAuthor);
begin
  try
    FDQuery.SQL.Text := 'INSERT INTO Authors (FirstName, LastName, ContactUrl, ResidentCountry, ResidentCity) ' +
                        'VALUES (:FirstName, :LastName, :ContactUrl, :ResidentCountry, :ResidentCity)';
    FDQuery.ParamByName('FirstName').AsString := Author.FirstName;
    FDQuery.ParamByName('LastName').AsString := Author.LastName;
    FDQuery.ParamByName('ContactUrl').AsString := Author.ContactUrl;
    FDQuery.ParamByName('ResidentCountry').AsString := Author.ResidentCountry;
    FDQuery.ParamByName('ResidentCity').AsString := Author.ResidentCity;

    // Submmit update to database
    FDQuery.ExecSQL;
    FDConnection.Commit;

    ShowMessage('Record inserted successfully.');
  except
    on E: Exception do
    begin
      FDConnection.Rollback;
      ShowMessage('Record inserted failed: ' + E.Message);
    end;
  end;

  // Refresh DBGrid
  ComboBoxSelectedTableNameChange(self);
end;

end.
