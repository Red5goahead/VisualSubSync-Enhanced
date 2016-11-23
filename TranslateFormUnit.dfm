object TranslateForm: TTranslateForm
  Left = 306
  Top = 321
  Width = 608
  Height = 289
  Caption = 'Translate'
  Color = clBtnFace
  Constraints.MinHeight = 280
  Constraints.MinWidth = 300
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnKeyUp = FormKeyUp
  PixelsPerInch = 96
  TextHeight = 13
  object PanelBottom: TPanel
    Left = 0
    Top = 227
    Width = 600
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      600
      35)
    object bttOK: TTntButton
      Left = 440
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'OK'
      TabOrder = 0
      OnClick = bttOKClick
    end
    object bttCancel: TTntButton
      Left = 520
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Cancel'
      TabOrder = 1
      OnClick = bttCancelClick
    end
  end
  object PanelClient: TPanel
    Left = 0
    Top = 0
    Width = 600
    Height = 227
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      600
      227)
    object rgSelectionType: TTntRadioGroup
      Left = 8
      Top = 8
      Width = 585
      Height = 81
      Anchors = [akLeft, akTop, akRight]
      Caption = ' Subtitles to translate '
      Items.Strings = (
        'All (create a new file)'
        'Missing only (enrich current file)')
      TabOrder = 0
      OnClick = rgSelectionTypeClick
    end
    object rgTextType: TTntRadioGroup
      Left = 8
      Top = 96
      Width = 585
      Height = 113
      Anchors = [akLeft, akTop, akRight]
      Caption = ' New subtitle text '
      Items.Strings = (
        'Empty'
        'Copy original text'
        'Copy tags only'
        'Custom text')
      TabOrder = 1
      OnClick = rgTextTypeClick
    end
    object edCustomText: TTntEdit
      Left = 152
      Top = 179
      Width = 433
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
      Text = '** untranslated **'
    end
    object edTargetFile: TTntEdit
      Left = 144
      Top = 26
      Width = 409
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
    end
    object bttBrowseTargetFile: TTntButton
      Left = 552
      Top = 24
      Width = 35
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '...'
      TabOrder = 4
      OnClick = bttBrowseTargetFileClick
    end
  end
  object TntSaveDialog1: TTntSaveDialog
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 736
    Top = 152
  end
end
