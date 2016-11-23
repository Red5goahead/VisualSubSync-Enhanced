object DiffSubsForm: TDiffSubsForm
  Left = 190
  Top = 219
  Width = 944
  Height = 557
  BorderIcons = [biSystemMenu]
  Caption = 'VisualSubSync Diff file'
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Arial'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  Position = poDefault
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 928
    Height = 23
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object lblFile1: TLabel
      Left = 0
      Top = 5
      Width = 33
      Height = 15
      Caption = ' Text : '
    end
    object lblFile2: TLabel
      Left = 381
      Top = 5
      Width = 58
      Height = 15
      Caption = ' VO/Other: '
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 479
    Width = 928
    Height = 19
    Panels = <
      item
        Width = 100
      end
      item
        Width = 100
      end
      item
        Width = 100
      end
      item
        Width = 100
      end>
  end
  object ResultGrid: TStringGrid
    Left = 0
    Top = 23
    Width = 928
    Height = 456
    Align = alClient
    ColCount = 4
    DefaultRowHeight = 17
    DefaultDrawing = False
    FixedCols = 0
    RowCount = 1
    FixedRows = 0
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    GridLineWidth = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goDrawFocusSelected, goRowSelect]
    ParentFont = False
    TabOrder = 2
    OnDrawCell = ResultGridDrawCell
  end
  object MainMenu: TMainMenu
    Left = 96
    Top = 73
    object File1: TMenuItem
      Caption = '&File'
      object Exit1: TTntMenuItem
        Caption = 'E&xit'
        OnClick = Exit1Click
      end
    end
    object mnuView: TMenuItem
      Caption = '&View'
      Enabled = False
      object PreviousChanges1: TTntMenuItem
        Caption = '&Previous Changes'
        ShortCut = 16464
        OnClick = PreviousChanges1Click
      end
      object NextChanges1: TTntMenuItem
        Caption = '&Next Changes'
        ShortCut = 16462
        OnClick = NextChanges1Click
      end
    end
    object Options1: TMenuItem
      Caption = '&Options'
      object mnuIgnoreCase: TMenuItem
        Caption = 'Ignore &Case'
        OnClick = mnuIgnoreCaseClick
      end
      object mnuIgnoreWhiteSpace: TMenuItem
        Caption = 'Ignore &White Space'
        OnClick = mnuIgnoreWhiteSpaceClick
      end
    end
  end
  object OpenDialog: TOpenDialog
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 183
    Top = 79
  end
end
