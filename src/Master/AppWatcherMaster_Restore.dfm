object DlgRestore: TDlgRestore
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'DlgRestore'
  ClientHeight = 306
  ClientWidth = 833
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnShow = FormShow
  TextHeight = 17
  object VersionsList: TListView
    Left = 0
    Top = 0
    Width = 833
    Height = 266
    Align = alTop
    Columns = <
      item
        AutoSize = True
        Caption = 'Nom'
      end
      item
        AutoSize = True
        Caption = 'Date'
      end
      item
        AutoSize = True
        Caption = 'Taille'
      end>
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
  end
  object BtnRestore: TButton
    Left = 377
    Top = 272
    Width = 75
    Height = 25
    Caption = 'Restaurer'
    Default = True
    TabOrder = 1
    OnClick = BtnRestoreClick
  end
  object BtnClose: TButton
    Left = 296
    Top = 272
    Width = 75
    Height = 25
    Caption = 'Fermer'
    ModalResult = 2
    TabOrder = 2
  end
end
