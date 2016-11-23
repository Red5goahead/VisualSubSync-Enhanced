object ProjectForm: TProjectForm
  Left = 339
  Top = 234
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'New Project'
  ClientHeight = 445
  ClientWidth = 675
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object bttCreateNewProject: TTntButton
    Left = 443
    Top = 407
    Width = 131
    Height = 26
    Caption = 'Create new project'
    TabOrder = 5
    OnClick = bttCreateNewProjectClick
  end
  object bttCancel: TTntButton
    Left = 579
    Top = 407
    Width = 75
    Height = 26
    Caption = 'Cancel'
    TabOrder = 7
    OnClick = bttCancelClick
  end
  object gbVideoFile: TTntGroupBox
    Left = 7
    Top = 8
    Width = 660
    Height = 89
    Caption = ' Video source file (optional) : '
    TabOrder = 0
    object bttBrowseVideoFile: TSpeedButton
      Left = 626
      Top = 16
      Width = 23
      Height = 22
      Caption = '...'
      OnClick = bttBrowseVideoFileClick
    end
    object bttVideoSourceOperation: TSpeedButton
      Left = 626
      Top = 40
      Width = 23
      Height = 22
      Hint = 'Start selected operation'
      Enabled = False
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        18000000000000030000C40E0000C40E00000000000000000000FFFFFFFFFFFF
        FEFEFEFCFCFCFBFBFBEDEEEEF7F7F7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFE
        FEFCFCFCFBFBFBFCFCFCFFFFFFFFFFFFCECECEAAAAAAE2E3E3696A6A515151F9
        F9F9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFEFEF8F8F8FBFBFBFFFFFFFFFFFF
        6666664D4D4D4D4D4D4C4D4D4C4C4C9A9A9AAAAAAAEFEFEFFFFFFFFFFFFFFFFF
        FFFFFFFFFEFEFEFCFCFCFDFDFDF1F1F16161614D4D4D6F6F6F8B8B8B5657574C
        4C4C4C4D4D999999FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFEFEBABABA4D4D4D
        4D4D4D8C8C8CFEFEFEFFFFFFEBEBEB5657574C4C4CDFE0E0FDFDFDFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFC1C1C15858584D4D4DD0D0D0FFFFFFFFFFFFFFFFFF8B
        8B8B4C4D4D9E9E9EF1F1F1FDFDFDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA4A4A4
        4D4D4DB3B3B3FFFFFFFFFFFFFDFDFD6F6F6F4D4D4D4C4D4DE5E5E5F5F5F5FDFD
        FDFFFFFFFFFFFFFFFFFFFDFDFD6868684D4D4D545454B1B1B1CECECE8989894D
        4D4D727272BABABADEDEDEF3F4F4DDDDDDF3F3F3FFFFFFFFFFFFFFFFFF919191
        7676764E4F4F4D4D4D4D4D4D4D4D4D4D4D4DB6B6B66262625656567171715959
        595A5A5AF0F0F0FFFFFFFFFFFFFEFEFEF8F8F8A8A8A84C4D4D777777A4A4A459
        5959BEBEBE5252525A5A5A8787875E5E5E4C4C4CDDDDDDFEFEFEFFFFFFFFFFFF
        FEFEFEE1E2E2A7A7A7DBDBDBFDFDFDE3E3E37D7D7D535353EAEAEAFFFFFFF1F1
        F15B5C5C6E6F6FD5D5D5FFFFFFFFFFFFFFFFFFFDFDFDF5F5F5F3F4F4F5F5F5BB
        BBBB4D4D4D727272FFFFFFFFFFFFFFFFFF8484844D4D4DA7A8A8FFFFFFFFFFFF
        FFFFFFFFFFFFFDFDFDF5F5F5F3F4F4E1E2E28989894F4F4FDEDEDEFFFFFFE8E8
        E8585858828282E4E4E4FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFEFEF8F8F8F7
        F7F7EEEFEF5454544F4F4F7070705353534D4D4DE7E7E7FEFEFEFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFEFEFEFCFCFCF7F7F77474745D5D5D8C8C8C6969
        69696969EFEFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFEFEFEF1F1F1FFFFFFF5F5F5FDFDFDFFFFFFFFFFFF}
      ParentShowHint = False
      ShowHint = True
      Visible = False
      OnClick = bttVideoSourceOperationClick
    end
    object EditVideoFilename: TTntEdit
      Left = 8
      Top = 17
      Width = 609
      Height = 21
      TabOrder = 0
    end
    object bttExtractWAVFromVideo: TTntButton
      Left = 7
      Top = 51
      Width = 611
      Height = 22
      Caption = 'Extract WAV/Peak file from video'
      TabOrder = 1
      OnClick = bttExtractWAVFromVideoClick
    end
    object cbbVideoSourceOperation: TComboBox
      Left = 8
      Top = 40
      Width = 609
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 2
      Visible = False
      OnSelect = cbbVideoSourceOperationSelect
    end
  end
  object gbWAVFile: TTntGroupBox
    Left = 7
    Top = 103
    Width = 660
    Height = 106
    Caption = ' Audio waveform / Audio only preview (optional) : '
    TabOrder = 1
    object bttBrowseWAVFile: TSpeedButton
      Left = 626
      Top = 73
      Width = 23
      Height = 22
      Caption = '...'
      OnClick = bttBrowseWAVFileClick
    end
    object bttBrowsePeakFile: TSpeedButton
      Left = 626
      Top = 45
      Width = 23
      Height = 22
      Caption = '...'
      OnClick = bttBrowsePeakFileClick
    end
    object EditWAVFilename: TTntEdit
      Left = 88
      Top = 74
      Width = 534
      Height = 21
      TabOrder = 0
      OnChange = EditUpdateColor
      OnEnter = EditPeakFilenameEnter
      OnExit = EditPeakFilenameExit
    end
    object rbExternal: TRadioButton
      Tag = 1
      Left = 8
      Top = 76
      Width = 73
      Height = 17
      Hint = 
        'Use an external or extracted WAV file for waveform and audio onl' +
        'y preview.'
      Caption = 'WAV file :'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = rbInternalWAVClick
    end
    object rbPeakOnly: TRadioButton
      Tag = 2
      Left = 8
      Top = 48
      Width = 73
      Height = 17
      Hint = 
        'Use the video file for audio preview and a created peak file for' +
        ' the waveform display.'
      Caption = 'Peak file :'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = rbInternalWAVClick
    end
    object EditPeakFilename: TTntEdit
      Left = 88
      Top = 46
      Width = 534
      Height = 21
      TabOrder = 3
      OnChange = EditUpdateColor
      OnEnter = EditPeakFilenameEnter
      OnExit = EditPeakFilenameExit
    end
    object rbNoWaveform: TTntRadioButton
      Left = 8
      Top = 24
      Width = 129
      Height = 17
      Hint = 
        'If available the audio track of the video file will be used for ' +
        'preview but no waveform is displayed.'
      Caption = 'No waveform display'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      OnClick = rbInternalWAVClick
    end
  end
  object gbSubtitleFile: TTntGroupBox
    Left = 7
    Top = 220
    Width = 660
    Height = 66
    Caption = ' Subtitle file : '
    TabOrder = 2
    object bttBrowseSubtitleFile: TSpeedButton
      Left = 626
      Top = 16
      Width = 23
      Height = 22
      Caption = '...'
      OnClick = bttBrowseSubtitleFileClick
    end
    object EditSubtitleFilename: TTntEdit
      Left = 8
      Top = 17
      Width = 529
      Height = 21
      TabOrder = 0
    end
    object cbSubtitleFormat: TTntComboBox
      Left = 544
      Top = 16
      Width = 73
      Height = 21
      Hint = 'Subtitle file format'
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      Text = 'SRT'
      OnChange = cbSubtitleFormatChange
      Items.Strings = (
        'SRT'
        'SSA'
        'ASS')
    end
    object chkSaveAsUTF8: TTntCheckBox
      Left = 8
      Top = 39
      Width = 92
      Height = 22
      Caption = 'Save as UTF8'
      TabOrder = 2
    end
    object chkLaunchTraslateActionOnCreate: TTntCheckBox
      Left = 118
      Top = 39
      Width = 183
      Height = 21
      Caption = 'Select translate mode on creation'
      Enabled = False
      TabOrder = 3
    end
  end
  object gbProjectFile: TTntGroupBox
    Left = 7
    Top = 346
    Width = 660
    Height = 50
    Caption = ' Project file : '
    TabOrder = 4
    object bttBrowseProjectFile: TSpeedButton
      Left = 626
      Top = 16
      Width = 23
      Height = 22
      Caption = '...'
      OnClick = bttBrowseProjectFileClick
    end
    object EditProjectFilename: TTntEdit
      Left = 8
      Top = 17
      Width = 609
      Height = 21
      TabOrder = 0
    end
  end
  object bttOk: TTntButton
    Left = 500
    Top = 407
    Width = 75
    Height = 26
    Caption = 'OK'
    TabOrder = 6
    Visible = False
    OnClick = bttOkClick
  end
  object gbVO: TTntGroupBox
    Left = 7
    Top = 291
    Width = 660
    Height = 50
    Caption = 'Reference VO/Other : '
    TabOrder = 3
    object bttBrowseSubtitleVO: TSpeedButton
      Left = 626
      Top = 16
      Width = 23
      Height = 22
      Caption = '...'
      OnClick = bttBrowseVOFileClick
    end
    object EditSubtitleVO: TTntEdit
      Left = 8
      Top = 16
      Width = 609
      Height = 21
      TabOrder = 0
    end
  end
  object TntOpenDialogBrowseGenericFile: TTntOpenDialog
    Left = 264
    Top = 343
  end
end
