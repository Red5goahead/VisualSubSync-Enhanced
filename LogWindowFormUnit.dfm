object LogForm: TLogForm
  Left = 204
  Top = 103
  Width = 505
  Height = 167
  BorderStyle = bsSizeToolWin
  Caption = 'Log window'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Microsoft Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  Menu = MainMenu1
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object MemoLog: TTntMemo
    Left = 0
    Top = 0
    Width = 489
    Height = 108
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object MainMenu1: TMainMenu
    Left = 8
    Top = 8
    object Window1: TMenuItem
      Caption = 'Window'
      object miClear: TMenuItem
        Caption = 'Clear'
        OnClick = miClearClick
      end
      object miStayHidden: TMenuItem
        AutoCheck = True
        Caption = 'Stay hidden'
        OnClick = miStayHiddenClick
      end
      object miClose: TMenuItem
        Caption = 'Close'
        OnClick = miCloseClick
      end
    end
    object Debug: TMenuItem
      Caption = 'Debug'
      object ListDirectShowfilters1: TMenuItem
        Caption = 'List used DirectShow filters'
        OnClick = ListDirectShowfilters1Click
      end
    end
  end
end
