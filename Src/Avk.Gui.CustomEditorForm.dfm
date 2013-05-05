inherited CustomEditorForm: TCustomEditorForm
  Left = 414
  Top = 193
  Caption = 'CustomEditorForm'
  ClientWidth = 442
  OldCreateOrder = True
  OnCloseQuery = FormCloseQuery
  ExplicitWidth = 450
  PixelsPerInch = 96
  TextHeight = 13
  object BottomPanel: TPanel
    Left = 0
    Top = 235
    Width = 442
    Height = 40
    Align = alBottom
    BevelOuter = bvNone
    Constraints.MinHeight = 32
    Constraints.MinWidth = 159
    TabOrder = 0
    DesignSize = (
      442
      40)
    object CancelButton: TcxButton
      Left = 365
      Top = 12
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = #1054#1090#1084#1077#1085#1072
      ModalResult = 2
      TabOrder = 0
      OnClick = CancelButtonClick
    end
    object OkButton: TcxButton
      Left = 286
      Top = 12
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = #1054#1050
      Default = True
      ModalResult = 1
      TabOrder = 1
      OnClick = OkButtonClick
    end
    object cxBottomLineGroupBox: TcxGroupBox
      Left = 0
      Top = 0
      Align = alTop
      TabOrder = 2
      Height = 8
      Width = 442
    end
  end
  object ClientPanel: TPanel
    Left = 0
    Top = 0
    Width = 442
    Height = 235
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object ScrollBox: TScrollBox
      Left = 0
      Top = 0
      Width = 442
      Height = 235
      Align = alClient
      BevelOuter = bvNone
      BorderStyle = bsNone
      TabOrder = 0
    end
  end
  object FormErrors: TFormErrors
    Items = <>
    OnRaiseError = FormErrorsRaiseError
    Left = 8
    Top = 8
  end
end
