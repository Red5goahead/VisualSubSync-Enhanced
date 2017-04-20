object SuggestionForm: TSuggestionForm
  Left = 209
  Top = 115
  Width = 273
  Height = 436
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSizeToolWin
  BorderWidth = 4
  Caption = 'Suggestions'
  Color = clBtnFace
  Constraints.MinHeight = 200
  Constraints.MinWidth = 100
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Microsoft Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  DesignSize = (
    249
    389)
  PixelsPerInch = 96
  TextHeight = 13
  object vtvSuggestionsLst: TVirtualDrawTree
    Left = 0
    Top = 0
    Width = 257
    Height = 321
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    Header.AutoSizeIndex = 0
    Header.Font.Charset = ANSI_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = '@Arial Unicode MS'
    Header.Font.Style = []
    Header.MainColumn = -1
    Header.Options = [hoColumnResize, hoDrag]
    ParentFont = False
    PopupMenu = TntPopupMenu1
    TabOrder = 0
    TreeOptions.SelectionOptions = [toMultiSelect]
    OnDblClick = vtvSuggestionsLstDblClick
    OnDrawNode = vtvSuggestionsLstDrawNode
    OnFocusChanged = vtvSuggestionsLstFocusChanged
    Columns = <>
  end
  object MemoSuggPreview: TTntMemo
    Left = 0
    Top = 328
    Width = 257
    Height = 73
    Anchors = [akLeft, akRight, akBottom]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    ReadOnly = True
    TabOrder = 1
  end
  object TntPopupMenu1: TTntPopupMenu
    Left = 8
    Top = 8
    object pmiClearSelected: TTntMenuItem
      Caption = 'Clear selected'
      OnClick = pmiClearSelectedClick
    end
    object pmiClearAll: TTntMenuItem
      Caption = 'Clear all'
      OnClick = pmiClearAllClick
    end
  end
end
