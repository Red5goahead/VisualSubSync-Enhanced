object DelayForm: TDelayForm
  Left = 378
  Top = 223
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  BorderWidth = 6
  Caption = 'Delay'
  ClientHeight = 289
  ClientWidth = 193
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Microsoft Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnKeyUp = FormKeyUp
  PixelsPerInch = 96
  TextHeight = 13
  object meDelay: TMaskEdit
    Left = 0
    Top = 52
    Width = 193
    Height = 21
    EditMask = '!99:99:99.999;1;0'
    MaxLength = 12
    TabOrder = 0
    Text = '  :  :  .   '
  end
  object rgApplyTo: TTntRadioGroup
    Left = 0
    Top = 80
    Width = 193
    Height = 81
    Caption = ' Apply to '
    ItemIndex = 0
    Items.Strings = (
      'All subtitles'
      'Selected subtitles'
      'All subtitles from cursor to end')
    TabOrder = 1
  end
  object rgType: TTntRadioGroup
    Left = 0
    Top = 0
    Width = 193
    Height = 41
    Caption = ' Type : '
    Columns = 2
    ItemIndex = 0
    Items.Strings = (
      'Positive (+)'
      'Negative (-)')
    TabOrder = 2
  end
  object bttApply: TTntButton
    Left = 8
    Top = 264
    Width = 81
    Height = 25
    Caption = 'Apply'
    TabOrder = 3
    OnClick = bttApplyClick
  end
  object bttCancel: TTntButton
    Left = 104
    Top = 264
    Width = 83
    Height = 25
    Caption = 'Cancel'
    TabOrder = 4
    OnClick = bttCancelClick
  end
  object rgShift: TTntRadioGroup
    Left = 0
    Top = 168
    Width = 193
    Height = 81
    Caption = ' Shift '
    ItemIndex = 0
    Items.Strings = (
      'Start and end time'
      'Start time only'
      'End time only')
    TabOrder = 5
  end
end
