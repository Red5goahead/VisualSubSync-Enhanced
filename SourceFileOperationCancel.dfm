object SourceFileOperationCancel: TSourceFileOperationCancel
  Left = 471
  Top = 345
  BorderIcons = []
  BorderStyle = bsSingle
  BorderWidth = 4
  Caption = 'Operation on video source file'
  ClientHeight = 84
  ClientWidth = 518
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object FFMpegOutputMessage: TLabel
    Left = 4
    Top = 34
    Width = 509
    Height = 13
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object VideoSourceOperationExecuteCancel: TTntButton
    Left = 0
    Top = 56
    Width = 513
    Height = 24
    Caption = 'Cancel'
    TabOrder = 0
    OnClick = VideoSourceOperationExecuteCancelClick
  end
  object FFMpegOutputProgressBar: TTntProgressBar
    Left = 3
    Top = 2
    Width = 510
    Height = 27
    Smooth = True
    Step = 5
    TabOrder = 1
  end
  object FakeWavDisplayerPanel: TPanel
    Left = 472
    Top = 37
    Width = 33
    Height = 15
    Color = clSkyBlue
    TabOrder = 2
    Visible = False
  end
  object StartTimer: TTimer
    Interval = 500
    OnTimer = StartTimerTimer
    Left = 424
    Top = 24
  end
end
