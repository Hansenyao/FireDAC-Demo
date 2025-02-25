object LoginDlg: TLoginDlg
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Login'
  ClientHeight = 155
  ClientWidth = 291
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnShow = FormShow
  TextHeight = 15
  object LabelUserName: TLabel
    Left = 24
    Top = 32
    Width = 61
    Height = 15
    Caption = 'User Name:'
  end
  object LabelPassword: TLabel
    Left = 24
    Top = 72
    Width = 53
    Height = 15
    Caption = 'Password:'
  end
  object ButtonOK: TButton
    Left = 40
    Top = 112
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 0
    OnClick = ButtonOKClick
  end
  object EditUserName: TEdit
    Left = 99
    Top = 24
    Width = 166
    Height = 23
    TabOrder = 1
  end
  object EditPassword: TEdit
    Left = 99
    Top = 64
    Width = 166
    Height = 23
    PasswordChar = '*'
    TabOrder = 2
  end
  object ButtonCancel: TButton
    Left = 160
    Top = 112
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 3
    OnClick = ButtonCancelClick
  end
end
