object BookDlg: TBookDlg
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Book'
  ClientHeight = 307
  ClientWidth = 390
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnClose = FormClose
  OnShow = FormShow
  TextHeight = 15
  object ButtonSubmmit: TButton
    Left = 112
    Top = 272
    Width = 75
    Height = 25
    Caption = 'Submmit'
    TabOrder = 0
    OnClick = ButtonSubmmitClick
  end
  object ButtonCancel: TButton
    Left = 208
    Top = 272
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = ButtonCancelClick
  end
end
