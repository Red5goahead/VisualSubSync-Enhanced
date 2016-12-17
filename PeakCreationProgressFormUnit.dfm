object PeakCreationProgressForm: TPeakCreationProgressForm
  Left = 334
  Top = 168
  AutoSize = True
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  BorderWidth = 4
  Caption = 'Creating peak file...'
  ClientHeight = 25
  ClientWidth = 261
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object ProgressBar1: TProgressBar
    Left = 0
    Top = 0
    Width = 261
    Height = 25
    TabOrder = 0
  end
end
