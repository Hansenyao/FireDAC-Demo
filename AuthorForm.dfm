object AuthorDlg: TAuthorDlg
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Author'
  ClientHeight = 207
  ClientWidth = 462
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnClose = FormClose
  OnDestroy = FormDestroy
  OnShow = FormShow
  TextHeight = 15
  object Label1: TLabel
    Left = 24
    Top = 32
    Width = 57
    Height = 15
    Alignment = taRightJustify
    Caption = 'FirstName:'
  end
  object Label2: TLabel
    Left = 248
    Top = 32
    Width = 56
    Height = 15
    Alignment = taRightJustify
    Caption = 'LastName:'
  end
  object Label3: TLabel
    Left = 35
    Top = 74
    Width = 46
    Height = 15
    Alignment = taRightJustify
    Caption = 'Country:'
  end
  object Label4: TLabel
    Left = 281
    Top = 74
    Width = 24
    Height = 15
    Alignment = taRightJustify
    Caption = 'City:'
  end
  object Label5: TLabel
    Left = 12
    Top = 116
    Width = 69
    Height = 15
    Caption = 'Contact URL:'
  end
  object ButtonSubmmit: TButton
    Left = 152
    Top = 166
    Width = 75
    Height = 25
    Caption = 'Submmit'
    TabOrder = 0
    OnClick = ButtonSubmmitClick
  end
  object ButtonCancel: TButton
    Left = 248
    Top = 166
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = ButtonCancelClick
  end
  object EditFirstName: TEdit
    Left = 87
    Top = 29
    Width = 121
    Height = 23
    TabOrder = 2
  end
  object EditLastName: TEdit
    Left = 311
    Top = 29
    Width = 121
    Height = 23
    TabOrder = 3
  end
  object EditCountry: TEdit
    Left = 87
    Top = 71
    Width = 121
    Height = 23
    TabOrder = 4
  end
  object EditCity: TEdit
    Left = 311
    Top = 71
    Width = 121
    Height = 23
    TabOrder = 5
  end
  object EditContactUrl: TEdit
    Left = 87
    Top = 113
    Width = 345
    Height = 23
    TabOrder = 6
  end
end
