object ResynchToolForm: TResynchToolForm
  Left = 516
  Top = 332
  BorderStyle = bsToolWindow
  Caption = 'Desynch Tool'
  ClientHeight = 381
  ClientWidth = 345
  Color = clBtnFace
  Constraints.MinHeight = 150
  Constraints.MinWidth = 240
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnDeactivate = FormDeactivate
  PixelsPerInch = 96
  TextHeight = 13
  object vtvPoSList: TVirtualDrawTree
    Left = 0
    Top = 0
    Width = 345
    Height = 335
    Align = alClient
    Constraints.MinHeight = 50
    Header.AutoSizeIndex = 0
    Header.DefaultHeight = 17
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'MS Sans Serif'
    Header.Font.Style = []
    Header.MainColumn = -1
    Header.Options = [hoColumnResize, hoDrag]
    PopupMenu = RTPopupMenu
    ScrollBarOptions.AlwaysVisible = True
    ScrollBarOptions.ScrollBars = ssVertical
    TabOrder = 0
    OnCompareNodes = vtvPoSListCompareNodes
    OnDblClick = vtvPoSListDblClick
    OnDrawNode = vtvPoSListDrawNode
    OnFocusChanged = vtvPoSListFocusChanged
    Columns = <>
  end
  object TntStatusBar1: TTntStatusBar
    Left = 0
    Top = 360
    Width = 345
    Height = 21
    Panels = <
      item
        Width = 50
      end>
  end
  object Panel1: TPanel
    Left = 0
    Top = 335
    Width = 345
    Height = 25
    Align = alBottom
    TabOrder = 2
    object GotoSelectedNodeButton: TButton
      Left = 3
      Top = 0
      Width = 160
      Height = 25
      Caption = 'Go to the selected sub'
      TabOrder = 0
      OnClick = GotoSelectedNodeButtonClick
    end
    object GotoSuggestedNodeButton: TButton
      Left = 165
      Top = 0
      Width = 160
      Height = 25
      Caption = 'Go to the suggested sub'
      Enabled = False
      TabOrder = 1
      OnClick = GotoSuggestedNodeButtonClick
    end
  end
  object RTPopupMenu: TTntPopupMenu
    Left = 16
    Top = 192
    object DeleteselectedPoS1: TTntMenuItem
      Caption = 'Delete selected PoS'
      OnClick = DeleteselectedPoS1Click
    end
  end
end
