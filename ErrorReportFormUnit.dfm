object ErrorReportForm: TErrorReportForm
  Left = 427
  Top = 255
  Width = 321
  Height = 425
  BorderStyle = bsSizeToolWin
  Caption = 'Error report'
  Color = clBtnFace
  Constraints.MinHeight = 100
  Constraints.MinWidth = 100
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Microsoft Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  Menu = TntMainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object vtvErrorList: TVirtualDrawTree
    Left = 0
    Top = 0
    Width = 305
    Height = 345
    Align = alClient
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Microsoft Sans Serif'
    Header.Font.Style = []
    Header.MainColumn = -1
    Header.Options = [hoColumnResize, hoDrag]
    PopupMenu = ErrorListPopupMenu
    TabOrder = 0
    TreeOptions.SelectionOptions = [toMultiSelect]
    OnDrawNode = vtvErrorListDrawNode
    OnFocusChanged = vtvErrorListFocusChanged
    Columns = <>
  end
  object TntStatusBar1: TTntStatusBar
    Left = 0
    Top = 345
    Width = 305
    Height = 21
    Panels = <
      item
        Width = 50
      end>
  end
  object ErrorListPopupMenu: TTntPopupMenu
    OnPopup = ErrorListPopupMenuPopup
    Left = 16
    Top = 48
    object pmiFixError: TTntMenuItem
      Action = MainForm.ActionFixSelectedErrors
    end
    object pmiFixAllXXX: TTntMenuItem
      Caption = 'Fix all errors of same type (color)'
      OnClick = pmiFixAllXXXClick
    end
    object pmiFixAllYYY: TTntMenuItem
      Caption = 'Fix all errors with same error msg'
      OnClick = pmiFixAllYYYClick
    end
    object N3: TTntMenuItem
      Caption = '-'
    end
    object pmiSelectAllXXX: TTntMenuItem
      Caption = 'Select all errors of same type (color)'
      OnClick = pmiSelectAllXXXClick
    end
    object pmiSelectAllYYY: TTntMenuItem
      Caption = 'Select all errors with same error msg'
      OnClick = pmiSelectAllYYYClick
    end
    object N5: TTntMenuItem
      Caption = '-'
    end
    object miClearAndSkip: TTntMenuItem
      Caption = 'Clear && Forget'
      OnClick = miClearAndSkipClick
    end
    object N4: TTntMenuItem
      Caption = '-'
    end
    object miClear: TTntMenuItem
      Caption = 'Clear'
      OnClick = miClearClick
    end
    object pmiClearAllYYY: TTntMenuItem
      Caption = 'Clear all errors with different error msg'
      OnClick = pmiClearAllYYYClick
    end
    object pmiClearAllXXX: TTntMenuItem
      Caption = 'Clear all errors with same error msg'
      OnClick = pmiClearAllXXXClick
    end
    object pmiClearAll: TTntMenuItem
      Caption = 'Clear all'
      OnClick = pmiClearAllClick
    end
  end
  object TntMainMenu1: TTntMainMenu
    Left = 16
    Top = 16
    object miFile: TTntMenuItem
      Caption = 'File'
      SubMenuImages = MainForm.ImageList1
      object miRecheck: TTntMenuItem
        Action = MainForm.ActionCheckErrors
      end
      object miClearAll: TTntMenuItem
        Caption = 'Clear all'
        OnClick = miClearAllClick
      end
      object miExportToHTML: TTntMenuItem
        Caption = 'Export to HTML...'
        OnClick = miExportToHTMLClick
      end
      object N2: TTntMenuItem
        Caption = '-'
      end
      object miPreferences: TTntMenuItem
        Action = MainForm.ActionErrorPreferences
      end
      object N1: TTntMenuItem
        Caption = '-'
      end
      object miClose: TTntMenuItem
        Caption = 'Close'
        OnClick = miCloseClick
      end
    end
  end
  object TntSaveDialog1: TTntSaveDialog
    Left = 16
    Top = 80
  end
end
