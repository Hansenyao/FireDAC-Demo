object MainDlg: TMainDlg
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Database Client Demo'
  ClientHeight = 444
  ClientWidth = 615
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnClose = FormClose
  OnShow = FormShow
  TextHeight = 15
  object LabelDriverID: TLabel
    Left = 8
    Top = 29
    Width = 45
    Height = 15
    Caption = 'DriverID:'
  end
  object LabelServer: TLabel
    Left = 304
    Top = 29
    Width = 35
    Height = 15
    Caption = 'Server:'
  end
  object LabelDatabase: TLabel
    Left = 8
    Top = 63
    Width = 51
    Height = 15
    Caption = 'Database:'
  end
  object LabelOSAuthent: TLabel
    Left = 304
    Top = 58
    Width = 61
    Height = 15
    Caption = 'OSAuthent:'
  end
  object LabelSelectTable: TLabel
    Left = 8
    Top = 188
    Width = 64
    Height = 15
    Caption = 'Select Table:'
  end
  object GroupBox2: TGroupBox
    Left = 2
    Top = 0
    Width = 605
    Height = 146
    Caption = 'Database'
    TabOrder = 8
  end
  object GroupBox1: TGroupBox
    Left = 2
    Top = 160
    Width = 605
    Height = 284
    Caption = 'Table'
    TabOrder = 7
    object ButtonEditData: TButton
      Left = 253
      Top = 18
      Width = 75
      Height = 25
      Caption = 'Edit'
      TabOrder = 0
      OnClick = ButtonEditDataClick
    end
    object ButtonAddData: TButton
      Left = 334
      Top = 18
      Width = 75
      Height = 25
      Caption = 'Add'
      TabOrder = 1
      OnClick = ButtonAddDataClick
    end
  end
  object DBGridTableData: TDBGrid
    Left = 8
    Top = 216
    Width = 593
    Height = 220
    Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect, dgConfirmDelete, dgCancelOnExit, dgTitleClick, dgTitleHotTrack]
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -12
    TitleFont.Name = 'Segoe UI'
    TitleFont.Style = []
  end
  object EditDriverID: TEdit
    Left = 97
    Top = 21
    Width = 145
    Height = 23
    TabOrder = 1
    Text = 'MSSQL'
  end
  object EditServer: TEdit
    Left = 393
    Top = 21
    Width = 200
    Height = 23
    TabOrder = 2
    Text = 'DESKTOP-GGJISR9\SQLEXPRESS'
  end
  object EditDatabase: TEdit
    Left = 97
    Top = 55
    Width = 145
    Height = 23
    TabOrder = 3
    Text = 'eBooks'
  end
  object ComboBoxOSAuthent: TComboBox
    Left = 393
    Top = 50
    Width = 200
    Height = 23
    Style = csDropDownList
    TabOrder = 4
    OnChange = ComboBoxOSAuthentChange
  end
  object ButtonConnect: TButton
    Left = 8
    Top = 100
    Width = 75
    Height = 25
    Caption = 'Connect'
    TabOrder = 5
    OnClick = ButtonConnectClick
  end
  object ComboBoxSelectedTableName: TComboBox
    Left = 97
    Top = 179
    Width = 145
    Height = 23
    Style = csDropDownList
    TabOrder = 6
    OnChange = ComboBoxSelectedTableNameChange
  end
  object FDConnection: TFDConnection
    Left = 488
    Top = 256
  end
  object FDQuery: TFDQuery
    Connection = FDConnection
    Left = 488
    Top = 328
  end
  object FDPhysMSSQLDriverLink: TFDPhysMSSQLDriverLink
    Left = 488
    Top = 392
  end
  object DataSource: TDataSource
    DataSet = FDQuery
    Left = 416
    Top = 336
  end
end
