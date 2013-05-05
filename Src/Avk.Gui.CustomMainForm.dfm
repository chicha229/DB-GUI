object CustomMainForm: TCustomMainForm
  Left = 0
  Top = 0
  Caption = 'CustomMainForm'
  ClientHeight = 254
  ClientWidth = 480
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object FormsPageControl: TcxPageControl
    Left = 144
    Top = 16
    Width = 289
    Height = 193
    TabOrder = 0
    Properties.CustomButtons.Buttons = <>
    Properties.HotTrack = True
    Properties.Options = [pcoAlwaysShowGoDialogButton, pcoCloseButton, pcoGoDialog, pcoGradient, pcoGradientClientArea, pcoRedrawOnResize]
    Properties.TabSlants.Kind = skCutCorner
    TabSlants.Kind = skCutCorner
    ClientRectBottom = 193
    ClientRectRight = 289
    ClientRectTop = 0
  end
  object TreeSplitter: TcxSplitter
    Left = 0
    Top = 0
    Width = 8
    Height = 254
    HotZoneClassName = 'TcxSimpleStyle'
  end
end
