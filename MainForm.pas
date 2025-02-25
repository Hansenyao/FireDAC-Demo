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
  LoginForm;

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
    procedure ButtonConnectClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ComboBox_SelectedTableNameChange(Sender: TObject);
    procedure ComboBoxOSAuthentChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    OSAuthent: Boolean;
    procedure ListAllTableNames();
  public
    { Public declarations }
  end;

var
  MainDlg: TMainDlg;

implementation

{$R *.dfm}

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
end;

procedure TMainDlg.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FDQuery.Close;
  FDConnection.Close;
end;

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

procedure TMainDlg.ComboBox_SelectedTableNameChange(Sender: TObject);
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
    FDQuery.Open();
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
end;

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
  finally
    TableList.Free;
  end;
end;


end.
