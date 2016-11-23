object DebugMain: TDebugMain
  Left = 193
  Top = 110
  Width = 781
  Height = 623
  Caption = 'Debug Window'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pnMsg: TPanel
    Left = 0
    Top = 0
    Width = 773
    Height = 49
    Align = alTop
    TabOrder = 0
    object pnError: TPanel
      Left = 1
      Top = 1
      Width = 771
      Height = 47
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      object Label1: TLabel
        Left = 8
        Top = 16
        Width = 71
        Height = 13
        Caption = 'Error Message:'
      end
      object lbMsg: TLabel
        Left = 88
        Top = 16
        Width = 3
        Height = 13
      end
    end
    object pnBreakpoint: TPanel
      Left = 1
      Top = 1
      Width = 771
      Height = 47
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object Label4: TLabel
        Left = 8
        Top = 16
        Width = 51
        Height = 13
        Caption = 'Breakpoint'
      end
    end
    object pnTrace: TPanel
      Left = 1
      Top = 1
      Width = 771
      Height = 47
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 2
      object Label7: TLabel
        Left = 8
        Top = 16
        Width = 28
        Height = 13
        Caption = 'Trace'
      end
    end
  end
  object pnData: TPanel
    Left = 0
    Top = 49
    Width = 773
    Height = 397
    Align = alClient
    TabOrder = 1
    object Splitter1: TSplitter
      Left = 454
      Top = 1
      Height = 392
      Align = alRight
      ResizeStyle = rsUpdate
    end
    object Splitter2: TSplitter
      Left = 1
      Top = 393
      Width = 771
      Height = 3
      Cursor = crVSplit
      Align = alBottom
      ResizeStyle = rsUpdate
    end
    object lbCode: TListBox
      Left = 1
      Top = 1
      Width = 453
      Height = 392
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier New'
      Font.Style = []
      ItemHeight = 14
      ParentFont = False
      TabOrder = 0
    end
    object pnStateVars: TPanel
      Left = 457
      Top = 1
      Width = 315
      Height = 392
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      object Label6: TLabel
        Left = 0
        Top = 124
        Width = 315
        Height = 13
        Align = alTop
        Caption = 'Variable State'
      end
      object Label5: TLabel
        Left = 0
        Top = 0
        Width = 315
        Height = 13
        Align = alTop
        Caption = 'Call Stack'
      end
      object tvVars: TTreeView
        Left = 0
        Top = 137
        Width = 315
        Height = 255
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        Indent = 19
        ParentFont = False
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
      end
      object lvCallstack: TListView
        Left = 0
        Top = 13
        Width = 315
        Height = 111
        Align = alTop
        Columns = <
          item
            Caption = 'Call'
            Width = 300
          end>
        HideSelection = False
        ReadOnly = True
        RowSelect = True
        ShowColumnHeaders = False
        TabOrder = 1
        ViewStyle = vsReport
        OnSelectItem = lvCallstackSelectItem
      end
    end
  end
  object pnEval: TPanel
    Left = 0
    Top = 446
    Width = 773
    Height = 143
    Align = alBottom
    TabOrder = 2
    Visible = False
    object Label2: TLabel
      Left = 6
      Top = 9
      Width = 45
      Height = 13
      Caption = 'Evaluate:'
    end
    object Label3: TLabel
      Left = 362
      Top = 9
      Width = 33
      Height = 13
      Caption = 'Result:'
    end
    object mmResult: TMemo
      Left = 362
      Top = 25
      Width = 305
      Height = 107
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      TabOrder = 0
    end
    object btRun: TButton
      Left = 320
      Top = 61
      Width = 35
      Height = 25
      Caption = '>>'
      TabOrder = 1
      OnClick = btRunClick
    end
    object mmEval: TMemo
      Left = 6
      Top = 25
      Width = 305
      Height = 107
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
    end
  end
end
