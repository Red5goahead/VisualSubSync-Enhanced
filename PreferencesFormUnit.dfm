object PreferencesForm: TPreferencesForm
  Left = 410
  Top = 143
  Width = 770
  Height = 590
  BorderIcons = [biSystemMenu]
  BorderWidth = 4
  Caption = 'Preferences'
  Color = clBtnFace
  Constraints.MinHeight = 590
  Constraints.MinWidth = 770
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Microsoft Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object PageControlPreferences: TTntPageControl
    Left = 0
    Top = 0
    Width = 746
    Height = 507
    ActivePage = tsGeneral
    Align = alClient
    TabOrder = 0
    object tsGeneral: TTntTabSheet
      BorderWidth = 4
      Caption = 'General'
      object Bevel2: TBevel
        Left = 0
        Top = 49
        Width = 730
        Height = 4
        Align = alTop
        Shape = bsSpacer
      end
      object Bevel3: TBevel
        Left = 0
        Top = 140
        Width = 730
        Height = 4
        Align = alTop
        Shape = bsSpacer
      end
      object GroupBoxWebServer: TTntGroupBox
        Left = 0
        Top = 0
        Width = 730
        Height = 49
        Align = alTop
        Caption = ' Web server : '
        TabOrder = 0
        object TntLabel1: TTntLabel
          Left = 16
          Top = 24
          Width = 25
          Height = 13
          Caption = 'Port :'
        end
        object EditServerPort: TTntEdit
          Left = 48
          Top = 20
          Width = 89
          Height = 21
          Hint = 'Port to which the webserver listen for connections'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          Text = '1'
        end
        object UpDownServerPort: TTntUpDown
          Left = 137
          Top = 20
          Width = 16
          Height = 21
          Associate = EditServerPort
          Min = 1
          Max = 32767
          Position = 1
          TabOrder = 1
          Thousands = False
        end
        object chkEnableCompression: TCheckBox
          Left = 280
          Top = 24
          Width = 121
          Height = 17
          Hint = 
            'Compress dynamic webpages for faster transmission (broken with I' +
            'E)'
          Caption = 'Enable compression'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 2
        end
      end
      object GroupBoxMisc: TGroupBox
        Left = 0
        Top = 144
        Width = 730
        Height = 327
        Align = alClient
        Caption = ' Misc : '
        TabOrder = 1
        object TntLabelMoveAfterSmartButtonIsUsed: TTntLabel
          Left = 45
          Top = 150
          Width = 234
          Height = 13
          Caption = 'Set cursor behind subtitle when smart is used (ms)'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Microsoft Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object LblDefaultPath: TTntLabel
          Left = 106
          Top = 181
          Width = 481
          Height = 17
          AutoSize = False
          Caption = 'Select path...'
          Enabled = False
          OnClick = LblDefaultPathClick
        end
        object chkAssociateExtVSSPRJ: TCheckBox
          Left = 16
          Top = 24
          Width = 265
          Height = 17
          Hint = 
            'Double click on a .vssprj file will automatically open the proje' +
            'ct in VisualSubSync'
          Caption = 'Associate project file (*.vssprj) with VisualSubSync'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          OnClick = chkAssociateExtVSSPRJClick
        end
        object chkSwapSubList: TCheckBox
          Left = 16
          Top = 75
          Width = 385
          Height = 17
          Caption = 'Swap subtitles list'
          TabOrder = 1
        end
        object chkAssociateExtSRT: TCheckBox
          Left = 16
          Top = 49
          Width = 209
          Height = 17
          Caption = 'Associate .SRT file with VisualSubSync'
          TabOrder = 2
          OnClick = chkAssociateExtSRTClick
        end
        object chkAssociateExtSSA: TCheckBox
          Left = 232
          Top = 49
          Width = 217
          Height = 17
          Caption = 'Associate .SSA file with VisualSubSync'
          TabOrder = 3
          OnClick = chkAssociateExtSSAClick
        end
        object chkAssociateExtASS: TCheckBox
          Left = 451
          Top = 49
          Width = 210
          Height = 17
          Caption = 'Associate .ASS file with VisualSubSync'
          TabOrder = 4
          OnClick = chkAssociateExtASSClick
        end
        object chkLoadMostRecentProjectOnStartup: TCheckBox
          Left = 16
          Top = 102
          Width = 201
          Height = 17
          Caption = 'Load most recent project on startup'
          TabOrder = 5
        end
        object ChkQuickSmartSilentZonePanels: TCheckBox
          Left = 16
          Top = 131
          Width = 225
          Height = 17
          Caption = 'Enable quick/smart and silent zone panels'
          TabOrder = 6
          OnClick = ChkQuickSmartSilentZonePanelsClick
        end
        object EditMoveAfterSmartButtonIsUsed: TTntEdit
          Left = 287
          Top = 148
          Width = 41
          Height = 21
          TabOrder = 7
          Text = '500'
        end
        object UpDownMoveAfterSmartButtonIsUsed: TTntUpDown
          Left = 328
          Top = 148
          Width = 16
          Height = 21
          Associate = EditMoveAfterSmartButtonIsUsed
          Max = 1500
          Increment = 10
          Position = 500
          TabOrder = 8
        end
        object ChkMoveAfterSmartButtonIsUsed: TCheckBox
          Left = 352
          Top = 149
          Width = 184
          Height = 17
          Alignment = taLeftJustify
          Caption = 'Auto play from new cursor position'
          TabOrder = 9
        end
        object ChkDefaultPath: TTntCheckBox
          Left = 16
          Top = 181
          Width = 89
          Height = 17
          Caption = 'Default Path:'
          TabOrder = 10
          OnClick = ChkDefaultPathClick
        end
        object ChkForceDefaultPath: TTntCheckBox
          Left = 37
          Top = 201
          Width = 188
          Height = 17
          Caption = 'Always use the default path'
          Enabled = False
          TabOrder = 11
        end
        object chkUTF8AsDefault: TCheckBox
          Left = 16
          Top = 226
          Width = 207
          Height = 17
          Caption = 'Set UTF-8 project creation as default'
          TabOrder = 12
        end
        object ChkUpdates: TCheckBox
          Left = 16
          Top = 281
          Width = 145
          Height = 17
          Caption = 'Check for updates on start'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 14
        end
        object chkAccentsAssistant: TCheckBox
          Left = 16
          Top = 252
          Width = 285
          Height = 20
          Caption = 'Accents/uppercase assistant (only for Italian dictionary)'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 13
        end
      end
      object GroupBoxBackup: TGroupBox
        Left = 0
        Top = 53
        Width = 730
        Height = 87
        Align = alTop
        Caption = ' Backup : '
        TabOrder = 2
        object Label6: TLabel
          Left = 16
          Top = 52
          Width = 153
          Height = 13
          Caption = 'Automatic save every (minutes) :'
        end
        object chkCreateBackup: TCheckBox
          Left = 16
          Top = 24
          Width = 185
          Height = 17
          Caption = 'Create backup file (*.bak) on save'
          TabOrder = 0
        end
        object bttOpenBackupDir: TButton
          Left = 288
          Top = 49
          Width = 90
          Height = 20
          Hint = 'Open backup directory in VisualSubSync directory'
          Caption = 'Show backups...'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          OnClick = bttOpenBackupDirClick
        end
        object EditBackupTime: TEdit
          Left = 176
          Top = 48
          Width = 57
          Height = 21
          TabOrder = 2
          Text = '0'
        end
        object UpDownBackupTime: TUpDown
          Left = 233
          Top = 48
          Width = 15
          Height = 21
          Associate = EditBackupTime
          TabOrder = 3
        end
        object chkAutoSaveWhenPlaying: TCheckBox
          Left = 232
          Top = 24
          Width = 169
          Height = 17
          Caption = 'Auto save when playing start'
          TabOrder = 4
        end
        object bttOpenBackupTempDir: TButton
          Left = 384
          Top = 49
          Width = 113
          Height = 20
          Hint = 'Open backup on any modification in TEMP directory'
          Caption = 'Show temp backups...'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 5
          OnClick = bttOpenBackupTempDirClick
        end
        object bttBackupConfiguration: TButton
          Left = 618
          Top = 49
          Width = 98
          Height = 20
          Hint = 'Save configuration and custom dictionary'
          Caption = 'Save configuration'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 6
          OnClick = bttBackupConfigurationClick
        end
        object bttRestoreConfiguration: TButton
          Left = 506
          Top = 49
          Width = 108
          Height = 20
          Hint = 'Restore configuration from saved file'
          Caption = 'Restore configuration'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 7
          OnClick = bttRestoreConfigurationClick
        end
      end
    end
    object TsPlayback: TTntTabSheet
      Caption = 'Playback'
      object TntGroupBoxPlaybackOptions: TTntGroupBox
        Left = 0
        Top = 0
        Width = 738
        Height = 479
        Align = alClient
        TabOrder = 0
        object labelMessagePlaybackAvailable: TLabel
          Left = 8
          Top = 456
          Width = 349
          Height = 13
          Caption = '** Most options are only available when your project is closed'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Microsoft Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          Visible = False
        end
        object TntLabelDelayPreviewVideo: TTntLabel
          Left = 269
          Top = 423
          Width = 142
          Height = 13
          Caption = 'Set subtitle preview delay (ms)'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Microsoft Sans Serif'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = False
        end
        object ChkUseInternalFilters: TCheckBox
          Left = 10
          Top = 21
          Width = 209
          Height = 17
          Caption = 'Prefer internal filters for rendering'
          TabOrder = 0
          OnClick = ChkUseInternalFiltersClick
        end
        object ChkDefaultInternalCodec: TRadioButton
          Left = 27
          Top = 143
          Width = 174
          Height = 17
          Caption = 'MPC-HC codec (discontinued)'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Microsoft Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 5
          OnClick = ChkDefaultInternalCodecClick
        end
        object ChkAlternativeInternalCodec: TRadioButton
          Left = 27
          Top = 66
          Width = 126
          Height = 17
          Caption = 'LAV codec'
          TabOrder = 1
          OnClick = ChkAlternativeInternalCodecClick
        end
        object GroupBoxLavVideoHwModes: TGroupBox
          Left = 177
          Top = 46
          Width = 371
          Height = 44
          Caption = 'Hardware Acceleration'
          TabOrder = 2
          object ChkLavVQuickSync: TRadioButton
            Left = 67
            Top = 16
            Width = 102
            Height = 18
            Caption = 'Intel Quick Sync'
            TabOrder = 0
          end
          object ChkLavVCuda: TRadioButton
            Left = 181
            Top = 16
            Width = 50
            Height = 18
            Caption = 'Cuda'
            TabOrder = 1
          end
          object ChkLavVDxva2: TRadioButton
            Left = 251
            Top = 16
            Width = 105
            Height = 18
            Caption = 'Dxva2(copy-back)'
            TabOrder = 2
          end
          object ChkLavVHwNone: TRadioButton
            Left = 8
            Top = 16
            Width = 50
            Height = 18
            Caption = 'None'
            TabOrder = 3
          end
        end
        object ChkDoubledSubResolution: TCheckBox
          Left = 268
          Top = 351
          Width = 137
          Height = 17
          Hint = 
            'Improve subtitles quality using Directvobsub upscaling feature (' +
            'only if video resolution is lower than 720p)'
          Caption = 'Improve subtitles quality'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 10
        end
        object ChkDisableDesktopComposition: TCheckBox
          Left = 10
          Top = 374
          Width = 162
          Height = 17
          Hint = 
            'Disable desktop composition/Aero feature in Windosws Vista & Win' +
            'dows Seven'
          Caption = 'Disable desktop composition'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 11
        end
        object ChkForceStopOnPausing: TCheckBox
          Left = 10
          Top = 351
          Width = 161
          Height = 17
          Caption = 'Force stop play when pausing'
          TabOrder = 9
        end
        object ChkUseReclockAudioRenderer: TCheckBox
          Left = 268
          Top = 374
          Width = 241
          Height = 17
          Caption = 'Use reclock audio renderer when available'
          TabOrder = 12
        end
        object GroupBoxCreateProject: TGroupBox
          Left = 29
          Top = 202
          Width = 382
          Height = 69
          Caption = 'Building new projects'
          TabOrder = 7
          object ChkDoNotUseInternalFiltersOnCreateProject: TCheckBox
            Left = 12
            Top = 20
            Width = 149
            Height = 17
            Caption = 'Do not use internal filters'
            TabOrder = 0
            OnClick = ChkDoNotUseInternalFiltersOnCreateProjectClick
          end
          object ChkUseDefaultCodecOnCreateProject: TCheckBox
            Left = 20
            Top = 44
            Width = 161
            Height = 17
            Caption = 'Force default audio codec'
            TabOrder = 1
            OnClick = ChkUseDefaultCodecOnCreateProjectClick
          end
          object ChkUseAlternativeCodecOnCreateProject: TCheckBox
            Left = 193
            Top = 44
            Width = 149
            Height = 17
            Caption = 'Force LAV audio codec'
            TabOrder = 2
            OnClick = ChkUseAlternativeCodecOnCreateProjectClick
          end
        end
        object ChkEnableLavAudioMixing: TCheckBox
          Left = 43
          Top = 111
          Width = 117
          Height = 17
          Caption = 'Enable audio mixing'
          TabOrder = 3
          OnClick = ChkUseAlternativeCodecOnCreateProjectClick
        end
        object ChkForceRegisteredCodecs: TCheckBox
          Left = 28
          Top = 176
          Width = 253
          Height = 17
          Caption = 'Prefer registered system codecs (when avaiable)'
          TabOrder = 6
          OnClick = ChkUseAlternativeCodecOnCreateProjectClick
        end
        object EditLavPreferredLanguages: TLabeledEdit
          Left = 176
          Top = 111
          Width = 145
          Height = 21
          EditLabel.Width = 95
          EditLabel.Height = 13
          EditLabel.Caption = 'Preferred languages'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 4
          Text = 'eng,ita,ger,fre'
        end
        object chkFlippedPicture: TCheckBox
          Left = 10
          Top = 398
          Width = 102
          Height = 17
          Hint = 'Check to flip picture'
          Caption = 'Flipped picture'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 13
        end
        object chkFlippedSubtitles: TCheckBox
          Left = 268
          Top = 398
          Width = 102
          Height = 17
          Hint = 'Check to flip subtitles'
          Caption = 'Flipped subtitles'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 14
        end
        object GroupBoxRenderer: TGroupBox
          Left = 29
          Top = 285
          Width = 382
          Height = 56
          Caption = 'Video renderer to use'
          TabOrder = 8
          object ChkPreferVmr9VideoRenderer: TRadioButton
            Left = 181
            Top = 24
            Width = 180
            Height = 17
            Caption = 'Video Mixing Renderer 9 (VMR-9)'
            TabOrder = 1
          end
          object ChkPreferVmr7VideoRenderer: TRadioButton
            Left = 7
            Top = 24
            Width = 170
            Height = 17
            Caption = 'Video Mixing Filter 7 (VMR-7)'
            Enabled = False
            TabOrder = 0
          end
        end
        object ChkForceWavDisplayPlayback: TCheckBox
          Left = 10
          Top = 423
          Width = 247
          Height = 17
          Caption = 'Force auto scroll/wav display starting playback'
          TabOrder = 15
        end
        object EditSubtitlePreviewDelay: TTntEdit
          Left = 420
          Top = 420
          Width = 41
          Height = 21
          Hint = 
            'Writing into subtitle edit box the result will be updated every ' +
            'N milliseconds according '
          ParentShowHint = False
          ShowHint = True
          TabOrder = 16
          Text = '750'
        end
        object UpDownSubtitlePreviewDelay: TTntUpDown
          Left = 461
          Top = 420
          Width = 16
          Height = 21
          Associate = EditSubtitlePreviewDelay
          Max = 2000
          Increment = 50
          Position = 750
          TabOrder = 17
        end
      end
    end
    object tsSubtitle: TTntTabSheet
      Caption = 'Subtitle'
      object TntGroupBox1: TTntGroupBox
        Left = 0
        Top = 0
        Width = 738
        Height = 480
        Align = alClient
        TabOrder = 0
        DesignSize = (
          738
          480)
        object TntLabel8: TTntLabel
          Left = 16
          Top = 164
          Width = 140
          Height = 13
          Caption = 'Blank between subtitles (ms) :'
        end
        object TntLabel13: TTntLabel
          Left = 8
          Top = 16
          Width = 731
          Height = 33
          Anchors = [akLeft, akTop, akRight]
          AutoSize = False
          Caption = 
            'Common settings used when editing subtitles in normal or timing ' +
            'mode, and possibly when fixing error with JavaScript plug-ins.'
          WordWrap = True
        end
        object TntLabel2: TTntLabel
          Left = 16
          Top = 68
          Width = 143
          Height = 13
          Caption = 'Characters per second target :'
        end
        object TntLabel7: TTntLabel
          Left = 16
          Top = 100
          Width = 146
          Height = 13
          Caption = 'Minimum subtitle duration (ms) :'
        end
        object TntLabel14: TTntLabel
          Left = 16
          Top = 132
          Width = 149
          Height = 13
          Caption = 'Maximum subtitle duration (ms) :'
        end
        object EditBlankBetweenSub: TTntEdit
          Left = 176
          Top = 160
          Width = 81
          Height = 21
          TabOrder = 3
          Text = '0'
        end
        object UpDownBlankBetweenSub: TTntUpDown
          Left = 257
          Top = 160
          Width = 15
          Height = 21
          Associate = EditBlankBetweenSub
          Max = 500
          Increment = 10
          TabOrder = 4
        end
        object EditCPSTarget: TTntEdit
          Left = 176
          Top = 64
          Width = 81
          Height = 21
          TabOrder = 0
          Text = '5'
        end
        object UpDownCPSTarget: TTntUpDown
          Left = 257
          Top = 64
          Width = 15
          Height = 21
          Associate = EditCPSTarget
          Min = 5
          Max = 50
          Position = 5
          TabOrder = 5
          Thousands = False
        end
        object EditMinimalDuration: TTntEdit
          Left = 176
          Top = 96
          Width = 81
          Height = 21
          TabOrder = 1
          Text = '500'
        end
        object UpDownMinimalDuration: TTntUpDown
          Left = 257
          Top = 96
          Width = 15
          Height = 21
          Associate = EditMinimalDuration
          Min = 200
          Max = 5000
          Increment = 100
          Position = 500
          TabOrder = 6
          Thousands = False
        end
        object EditMaximalDuration: TTntEdit
          Left = 176
          Top = 128
          Width = 81
          Height = 21
          TabOrder = 2
          Text = '6000'
        end
        object UpDownMaximumDuration: TTntUpDown
          Left = 257
          Top = 128
          Width = 16
          Height = 21
          Associate = EditMaximalDuration
          Min = 1000
          Max = 25000
          Increment = 100
          Position = 6000
          TabOrder = 7
          Thousands = False
        end
        object ChkCustomText: TTntCheckBox
          Left = 16
          Top = 203
          Width = 137
          Height = 17
          Caption = 'Set custom default text'
          TabOrder = 8
          OnClick = ChkCustomTextClick
        end
        object EditCustomText: TTntEdit
          Left = 160
          Top = 200
          Width = 121
          Height = 21
          TabOrder = 9
          Visible = False
        end
      end
    end
    object tsErrorChecking: TTntTabSheet
      BorderWidth = 4
      Caption = 'Error checking'
      object Bevel7: TBevel
        Left = 0
        Top = 268
        Width = 730
        Height = 4
        Align = alBottom
        Shape = bsSpacer
      end
      object Bevel8: TBevel
        Left = 0
        Top = 264
        Width = 730
        Height = 4
        Align = alBottom
        Shape = bsSpacer
      end
      object ListErrorChecking: TTntCheckListBox
        Left = 0
        Top = 0
        Width = 730
        Height = 264
        OnClickCheck = ListErrorCheckingClickCheck
        Align = alClient
        ItemHeight = 13
        PopupMenu = pmErrorChecking
        Sorted = True
        TabOrder = 0
        OnClick = ListErrorCheckingClick
      end
      object Panel2: TPanel
        Left = 0
        Top = 272
        Width = 730
        Height = 88
        Align = alBottom
        BevelOuter = bvLowered
        TabOrder = 1
        DesignSize = (
          730
          88)
        object TntLabel3: TTntLabel
          Left = 8
          Top = 7
          Width = 72
          Height = 13
          Caption = 'Description :'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Microsoft Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lbErrorDescription: TTntLabel
          Left = 8
          Top = 21
          Width = 658
          Height = 59
          Anchors = [akLeft, akTop, akRight, akBottom]
          AutoSize = False
          Caption = '-'
          WordWrap = True
        end
        object TntLabel4: TTntLabel
          Left = 673
          Top = 4
          Width = 57
          Height = 13
          Alignment = taCenter
          Anchors = [akTop, akRight]
          AutoSize = False
          Caption = 'Color :'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Microsoft Sans Serif'
          Font.Style = []
          ParentFont = False
          Transparent = True
        end
        object ShapeErrorColor: TShape
          Left = 689
          Top = 24
          Width = 25
          Height = 25
          Anchors = [akTop, akRight]
          Shape = stCircle
        end
        object Bevel1: TBevel
          Left = 665
          Top = 8
          Width = 9
          Height = 72
          Anchors = [akTop, akRight, akBottom]
          Shape = bsRightLine
        end
      end
      object ListPluginParam: TVirtualStringTree
        Left = 0
        Top = 360
        Width = 730
        Height = 112
        Align = alBottom
        AnimationDuration = 1
        EditDelay = 40
        Header.AutoSizeIndex = 1
        Header.Font.Charset = DEFAULT_CHARSET
        Header.Font.Color = clWindowText
        Header.Font.Height = -11
        Header.Font.Name = 'Microsoft Sans Serif'
        Header.Font.Style = []
        Header.MainColumn = 1
        Header.Options = [hoAutoResize, hoColumnResize, hoVisible, hoAutoSpring]
        Header.Style = hsPlates
        HintMode = hmHint
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        TreeOptions.AnimationOptions = [toAnimatedToggle]
        TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning]
        TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages]
        TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toCenterScrollIntoView]
        TreeOptions.StringOptions = [toAutoAcceptEditChange]
        OnCreateEditor = ListPluginParamCreateEditor
        OnEditing = ListPluginParamEditing
        OnGetText = ListPluginParamGetText
        OnGetHint = ListPluginParamGetHint
        Columns = <
          item
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring]
            Position = 0
            Width = 248
            WideText = 'Parameter'
          end
          item
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring]
            Position = 1
            Width = 294
            WideText = 'Value'
          end
          item
            Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring]
            Position = 2
            Width = 184
            WideText = 'Unit'
          end>
      end
    end
    object tsHotKeys: TTntTabSheet
      BorderWidth = 4
      Caption = 'Hotkeys'
      object Bevel9: TBevel
        Left = 0
        Top = 433
        Width = 730
        Height = 4
        Align = alBottom
        Shape = bsSpacer
      end
      object ListHotkeys: TTntListView
        Left = 0
        Top = 0
        Width = 730
        Height = 433
        Align = alClient
        Columns = <
          item
            Caption = 'Name'
            Width = 250
          end
          item
            Caption = 'Normal'
            Width = 100
          end
          item
            Caption = 'Timing'
            Width = 100
          end>
        ColumnClick = False
        HideSelection = False
        ReadOnly = True
        RowSelect = True
        SortType = stText
        TabOrder = 0
        ViewStyle = vsReport
        OnDeletion = ListHotkeysDeletion
        OnSelectItem = ListHotkeysSelectItem
      end
      object PanelHotKey: TPanel
        Left = 0
        Top = 437
        Width = 730
        Height = 35
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 1
        object TntLabel5: TTntLabel
          Left = 1
          Top = 0
          Width = 33
          Height = 13
          Caption = 'Mode :'
        end
        object TntLabel6: TTntLabel
          Left = 128
          Top = 0
          Width = 49
          Height = 13
          AutoSize = False
          Caption = 'Hot key :'
        end
        object ComboHotkeyMode: TTntComboBox
          Left = 0
          Top = 13
          Width = 113
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          ItemIndex = 0
          TabOrder = 0
          Text = 'Normal'
          OnSelect = ComboHotkeyModeSelect
          Items.Strings = (
            'Normal'
            'Timing')
        end
        object HotKey1: THotKey
          Left = 128
          Top = 13
          Width = 457
          Height = 21
          HotKey = 0
          InvalidKeys = [hcNone]
          Modifiers = []
          TabOrder = 1
        end
        object bttSetHotkey: TButton
          Left = 595
          Top = 12
          Width = 33
          Height = 22
          Caption = 'Set'
          TabOrder = 2
          OnClick = bttSetHotkeyClick
        end
        object bttClearHotkey: TButton
          Left = 632
          Top = 12
          Width = 34
          Height = 22
          Caption = 'Clear'
          TabOrder = 3
          OnClick = bttClearHotkeyClick
        end
        object bttResetAllHotkeys: TButton
          Left = 669
          Top = 12
          Width = 53
          Height = 22
          Caption = 'Reset all'
          TabOrder = 4
          OnClick = bttResetAllHotkeysClick
        end
      end
    end
    object tsMouse: TTntTabSheet
      BorderWidth = 4
      Caption = 'Mouse'
      object Bevel4: TBevel
        Left = 0
        Top = 121
        Width = 730
        Height = 4
        Align = alTop
        Shape = bsSpacer
      end
      object GroupBox2: TGroupBox
        Left = 0
        Top = 0
        Width = 730
        Height = 121
        Align = alTop
        Caption = ' Mouse wheel control : '
        TabOrder = 0
        object Label1: TLabel
          Left = 24
          Top = 44
          Width = 93
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Time scrolling :'
        end
        object Label2: TLabel
          Left = 24
          Top = 68
          Width = 93
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Vertical Zoom :'
        end
        object Label3: TLabel
          Left = 24
          Top = 92
          Width = 93
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Horizontal Zoom :'
        end
        object Label4: TLabel
          Left = 24
          Top = 22
          Width = 93
          Height = 13
          Alignment = taCenter
          AutoSize = False
          Caption = 'Actions'
          Color = clWindowText
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindow
          Font.Height = -11
          Font.Name = 'Microsoft Sans Serif'
          Font.Style = [fsBold]
          ParentColor = False
          ParentFont = False
          Transparent = False
        end
        object Label5: TLabel
          Left = 124
          Top = 22
          Width = 261
          Height = 13
          Alignment = taCenter
          AutoSize = False
          Caption = 'Modifiers'
          Color = clWindowText
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindow
          Font.Height = -11
          Font.Name = 'Microsoft Sans Serif'
          Font.Style = [fsBold]
          ParentColor = False
          ParentFont = False
          Transparent = False
        end
        object ComboWheelTimeScrollModifier: TComboBox
          Left = 124
          Top = 40
          Width = 261
          Height = 21
          Style = csDropDownList
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Microsoft Sans Serif'
          Font.Style = []
          ItemHeight = 13
          ParentFont = False
          TabOrder = 0
          Items.Strings = (
            'Shift'
            'Alt'
            'Ctrl'
            'None')
        end
        object ComboWheelVZoomModifier: TComboBox
          Left = 124
          Top = 64
          Width = 261
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 1
          Items.Strings = (
            'Shift'
            'Alt'
            'Ctrl'
            'None')
        end
        object ComboWheelHZoomModifier: TComboBox
          Left = 124
          Top = 88
          Width = 261
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 2
          Items.Strings = (
            'Shift'
            'Alt'
            'Ctrl'
            'None')
        end
      end
      object GroupBox3: TGroupBox
        Left = 0
        Top = 125
        Width = 730
        Height = 347
        Align = alClient
        Caption = ' Misc : '
        TabOrder = 1
        object chkEnableSSATimingMode: TCheckBox
          Left = 16
          Top = 24
          Width = 385
          Height = 17
          Caption = 
            'Left/right mouse button set start/stop time in Timing Mode (SSA ' +
            'mouse mode)'
          TabOrder = 0
        end
        object chkEnableMouseSnapping: TCheckBox
          Left = 16
          Top = 40
          Width = 385
          Height = 17
          Caption = 'Enable mouse snapping'
          TabOrder = 1
        end
        object chkEnableMouseAntiOverlapping: TCheckBox
          Left = 16
          Top = 56
          Width = 385
          Height = 17
          Caption = 'Enable mouse anti-overlapping'
          TabOrder = 2
        end
      end
    end
    object tsFonts: TTntTabSheet
      BorderWidth = 4
      Caption = 'Fonts'
      object Bevel5: TBevel
        Left = 0
        Top = 81
        Width = 730
        Height = 4
        Align = alTop
        Shape = bsSpacer
      end
      object TntGroupBox2: TTntGroupBox
        Left = 0
        Top = 0
        Width = 730
        Height = 81
        Align = alTop
        Caption = ' Subtitles list : '
        TabOrder = 0
        DesignSize = (
          730
          81)
        object EditSubListFont: TTntEdit
          Left = 16
          Top = 24
          Width = 651
          Height = 41
          Anchors = [akLeft, akTop, akRight]
          AutoSize = False
          ReadOnly = True
          TabOrder = 0
          Text = 'ABCabc 123'
        end
        object bttSubListFont: TTntButton
          Left = 674
          Top = 24
          Width = 33
          Height = 25
          Anchors = [akTop, akRight]
          Caption = '...'
          TabOrder = 1
          OnClick = bttSubListFontClick
        end
      end
      object TntGroupBox3: TTntGroupBox
        Left = 0
        Top = 85
        Width = 730
        Height = 100
        Align = alTop
        Caption = ' Subtitles text : '
        TabOrder = 1
        DesignSize = (
          730
          100)
        object EditSubTextFont: TTntEdit
          Left = 16
          Top = 24
          Width = 657
          Height = 41
          Anchors = [akLeft, akTop, akRight]
          AutoSize = False
          ReadOnly = True
          TabOrder = 0
          Text = 'ABCabc 123'
        end
        object bttSubTextFont: TTntButton
          Left = 680
          Top = 24
          Width = 33
          Height = 25
          Anchors = [akTop, akRight]
          Caption = '...'
          TabOrder = 1
          OnClick = bttSubTextFontClick
        end
      end
      object TntGroupBox10: TTntGroupBox
        Left = 0
        Top = 185
        Width = 730
        Height = 287
        Align = alClient
        Caption = ' Subtitles video : '
        TabOrder = 2
        DesignSize = (
          730
          287)
        object TntLabel25: TTntLabel
          Left = 16
          Top = 128
          Width = 33
          Height = 13
          Caption = 'Outline'
        end
        object TntLabel26: TTntLabel
          Left = 16
          Top = 173
          Width = 39
          Height = 13
          Caption = 'Shadow'
        end
        object ShapeSubVideoOutline: TTntShape
          Left = 139
          Top = 126
          Width = 17
          Height = 17
          Hint = 'Set outline color'
          Brush.Color = clBlack
          ParentShowHint = False
          Shape = stRoundSquare
          ShowHint = True
          OnMouseDown = ShapeSubVideoOutlineMouseDown
        end
        object ShapeSubVideoShadow: TTntShape
          Left = 139
          Top = 171
          Width = 17
          Height = 17
          Hint = 'Set shadow color'
          Brush.Color = 1315860
          ParentShowHint = False
          Shape = stRoundSquare
          ShowHint = True
          OnMouseDown = ShapeSubVideoShadowMouseDown
        end
        object TntLabel27: TTntLabel
          Left = 16
          Top = 82
          Width = 65
          Height = 13
          Caption = 'Transparency'
        end
        object EditSubVideoFont: TTntEdit
          Left = 16
          Top = 24
          Width = 657
          Height = 52
          Anchors = [akLeft, akTop, akRight]
          AutoSize = False
          ReadOnly = True
          TabOrder = 0
          Text = 'ABCabc 123'
        end
        object bttSubVideoFont: TTntButton
          Left = 680
          Top = 24
          Width = 33
          Height = 25
          Anchors = [akTop, akRight]
          Caption = '...'
          TabOrder = 1
          OnClick = bttSubVideoFontClick
        end
        object EditOutlineSubVideoFont: TEdit
          Left = 89
          Top = 124
          Width = 22
          Height = 21
          Hint = 'Outline'
          ParentShowHint = False
          ReadOnly = True
          ShowHint = True
          TabOrder = 2
          Text = '3'
        end
        object UpDownSubVideoOutline: TTntUpDown
          Left = 111
          Top = 124
          Width = 16
          Height = 21
          Associate = EditOutlineSubVideoFont
          Max = 9
          Position = 3
          TabOrder = 3
          Thousands = False
        end
        object EditShadowSubVideoFont: TEdit
          Left = 89
          Top = 169
          Width = 22
          Height = 21
          Hint = 'Shadow'
          ParentShowHint = False
          ReadOnly = True
          ShowHint = True
          TabOrder = 4
          Text = '4'
        end
        object UpDownSubVideoShadow: TTntUpDown
          Left = 111
          Top = 169
          Width = 16
          Height = 21
          Associate = EditShadowSubVideoFont
          Max = 9
          Position = 4
          TabOrder = 5
          Thousands = False
        end
        object ButtonDefaultSubtitlesVideo: TTntButton
          Left = 592
          Top = 88
          Width = 75
          Height = 25
          Caption = 'Default'
          TabOrder = 6
          OnClick = ButtonDefaultSubtitlesVideoClick
        end
        object TrackBarTransparencySubVideo: TTntTrackBar
          Left = 82
          Top = 76
          Width = 150
          Height = 30
          Hint = 'Set subtitle video transparency'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 7
        end
        object TrackBarTransparencyOutline: TTntTrackBar
          Left = 166
          Top = 122
          Width = 150
          Height = 30
          Hint = 'Set outline transparency'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 8
        end
        object TrackBarTransparencyShadow: TTntTrackBar
          Left = 166
          Top = 169
          Width = 150
          Height = 30
          Hint = 'Set shadow transparency'
          ParentShowHint = False
          Position = 2
          ShowHint = True
          TabOrder = 9
        end
      end
    end
    object tsTimingMode: TTntTabSheet
      BorderWidth = 4
      Caption = 'Timing mode'
      object TntGroupBox4: TTntGroupBox
        Left = 0
        Top = 0
        Width = 730
        Height = 472
        Align = alClient
        TabOrder = 0
        object chkEnableSubCreationWithSpaceKey: TCheckBox
          Left = 16
          Top = 56
          Width = 385
          Height = 17
          Caption = 'Enable subtitle creation with space key (toggle) in timing mode'
          TabOrder = 0
        end
        object chkSpaceKeyModifyTiming: TCheckBox
          Left = 48
          Top = 73
          Width = 217
          Height = 17
          Caption = 'Space key modify existing subtitles timing'
          TabOrder = 1
        end
        object chkDisableSubEditionInTimingMode: TCheckBox
          Left = 16
          Top = 24
          Width = 385
          Height = 17
          Caption = 'Disable subtitle edition in timing mode'
          TabOrder = 2
        end
      end
    end
    object tsWAVDisplay: TTntTabSheet
      BorderWidth = 4
      Caption = 'WAV display'
      object Bevel6: TBevel
        Left = 0
        Top = 241
        Width = 730
        Height = 4
        Align = alTop
        Shape = bsSpacer
      end
      object Bevel10: TBevel
        Left = 0
        Top = 121
        Width = 730
        Height = 4
        Align = alTop
        Shape = bsSpacer
      end
      object TntGroupBox5: TTntGroupBox
        Left = 0
        Top = 0
        Width = 730
        Height = 121
        Align = alTop
        Caption = ' Scene change : '
        TabOrder = 0
        object TntLabel9: TTntLabel
          Left = 16
          Top = 54
          Width = 96
          Height = 13
          Caption = 'Safety zone offsets :'
        end
        object TntLabel10: TTntLabel
          Left = 24
          Top = 76
          Width = 22
          Height = 13
          Caption = 'Start'
        end
        object TntLabel11: TTntLabel
          Left = 134
          Top = 76
          Width = 22
          Height = 13
          Caption = 'Stop'
        end
        object TntLabel12: TTntLabel
          Left = 256
          Top = 54
          Width = 94
          Height = 13
          Caption = 'Filter inside subtitle :'
        end
        object chkSceneChange: TCheckBox
          Left = 16
          Top = 24
          Width = 121
          Height = 17
          Caption = 'Show scene change'
          TabOrder = 0
        end
        object EditSCStartOffset: TEdit
          Left = 51
          Top = 72
          Width = 57
          Height = 21
          Hint = 'Start offset in ms'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          Text = '130'
        end
        object UpDownSCStart: TTntUpDown
          Left = 108
          Top = 72
          Width = 15
          Height = 21
          Associate = EditSCStartOffset
          Max = 2000
          Increment = 10
          Position = 130
          TabOrder = 2
          Thousands = False
        end
        object EditSCStopOffset: TEdit
          Left = 161
          Top = 72
          Width = 57
          Height = 21
          Hint = 'Stop offset in ms'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 3
          Text = '130'
        end
        object UpDownSCStop: TTntUpDown
          Left = 218
          Top = 72
          Width = 15
          Height = 21
          Associate = EditSCStopOffset
          Max = 2000
          Increment = 10
          Position = 130
          TabOrder = 4
          Thousands = False
        end
        object EditSCFilterOffset: TEdit
          Left = 256
          Top = 72
          Width = 57
          Height = 21
          Hint = 'Offset from start and stop in ms (0 to disable)'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 5
          Text = '250'
        end
        object UpDownSCFilter: TTntUpDown
          Left = 313
          Top = 72
          Width = 15
          Height = 21
          Associate = EditSCFilterOffset
          Max = 2000
          Increment = 10
          Position = 250
          TabOrder = 6
          Thousands = False
        end
      end
      object TntGroupBox6: TTntGroupBox
        Left = 0
        Top = 125
        Width = 730
        Height = 116
        Align = alTop
        Caption = ' Misc : '
        TabOrder = 1
        object chkShowTextInWAVDisplay: TCheckBox
          Left = 16
          Top = 24
          Width = 113
          Height = 17
          Caption = 'Show subtitle text'
          TabOrder = 0
        end
        object chkDisableVOShowTextInWAVDisplay: TCheckBox
          Left = 16
          Top = 44
          Width = 217
          Height = 17
          Caption = 'Do not show VO text also when available'
          TabOrder = 1
        end
        object chkShowSpecialTagSubs: TCheckBox
          Left = 16
          Top = 64
          Width = 177
          Height = 17
          Hint = 
            'The special tags subtitles contains the character { } with tags ' +
            'inside and starts with the first one.'
          Caption = 'Show special tags subtitles text'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 2
        end
        object chkSwapSubVO: TCheckBox
          Left = 16
          Top = 84
          Width = 217
          Height = 17
          Caption = 'Swap Subtitles\VO on wav display'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 3
        end
        object chkEnableShowVideoForNewProject: TCheckBox
          Left = 264
          Top = 24
          Width = 240
          Height = 17
          Caption = 'Enable show video (default for new project)'
          TabOrder = 4
        end
        object chkEnableDetachedVideoForNewProject: TCheckBox
          Left = 264
          Top = 44
          Width = 240
          Height = 17
          Caption = 'Enable detach video (default for new project)'
          TabOrder = 5
        end
      end
      object TntGroupBox9: TTntGroupBox
        Left = 0
        Top = 245
        Width = 730
        Height = 227
        Align = alClient
        Caption = 'WavDisplayer Colors'
        TabOrder = 2
        object TntLabel19: TTntLabel
          Left = 20
          Top = 25
          Width = 80
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Wave:'
        end
        object ShapeWC: TTntShape
          Left = 105
          Top = 23
          Width = 17
          Height = 17
          Shape = stRoundSquare
          OnMouseDown = ShapeWCMouseDown
        end
        object TntLabel20: TTntLabel
          Left = 20
          Top = 45
          Width = 80
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Background:'
        end
        object ShapeWBC: TTntShape
          Left = 105
          Top = 43
          Width = 17
          Height = 17
          Shape = stRoundSquare
          OnMouseDown = ShapeWCMouseDown
        end
        object TntLabel21: TTntLabel
          Left = 20
          Top = 65
          Width = 80
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Cursor:'
        end
        object ShapeCC: TTntShape
          Left = 105
          Top = 63
          Width = 17
          Height = 17
          Shape = stRoundSquare
          OnMouseDown = ShapeWCMouseDown
        end
        object TntLabel22: TTntLabel
          Left = 164
          Top = 25
          Width = 80
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Subtitle 1:'
        end
        object ShapeRC1: TTntShape
          Left = 249
          Top = 23
          Width = 17
          Height = 17
          Shape = stRoundSquare
          OnMouseDown = ShapeWCMouseDown
        end
        object TntLabel23: TTntLabel
          Left = 164
          Top = 45
          Width = 80
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Subtitle 2:'
        end
        object ShapeRC2: TTntShape
          Left = 249
          Top = 43
          Width = 17
          Height = 17
          Shape = stRoundSquare
          OnMouseDown = ShapeWCMouseDown
        end
        object TntLabel24: TTntLabel
          Left = 164
          Top = 65
          Width = 80
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Subtitle VO:'
        end
        object ShapeRCNE: TTntShape
          Left = 249
          Top = 63
          Width = 17
          Height = 17
          Shape = stRoundSquare
          OnMouseDown = ShapeWCMouseDown
        end
        object ButtonDefaultWavColours: TTntButton
          Left = 344
          Top = 24
          Width = 75
          Height = 25
          Caption = 'Default'
          TabOrder = 0
          OnClick = ButtonDefaultWavColoursClick
        end
      end
    end
    object tsDesynchTools: TTntTabSheet
      Caption = 'Desynch Tools'
      object TntGroupBox7: TTntGroupBox
        Left = 0
        Top = 0
        Width = 738
        Height = 480
        Align = alClient
        TabOrder = 0
        object ChkDesynchToolsAutoReset: TCheckBox
          Left = 8
          Top = 25
          Width = 234
          Height = 17
          Caption = 'Auto-Reset'
          TabOrder = 0
        end
        object ChkDesynchUseColorsForSubtitles: TCheckBox
          Left = 8
          Top = 49
          Width = 234
          Height = 17
          Caption = 'Use colors for desynch checkpoint zones'
          TabOrder = 1
        end
        object ChkDesynchUseIconsForSubtitles: TCheckBox
          Left = 8
          Top = 73
          Width = 234
          Height = 17
          Caption = 'Use icons for checkpoints'
          TabOrder = 2
        end
        object ChkDesynchAssumeFirstSubtitleSynched: TCheckBox
          Left = 8
          Top = 97
          Width = 234
          Height = 17
          Caption = 'Assume first subtitle as synched'
          TabOrder = 3
        end
        object ChkDesynchLog: TCheckBox
          Left = 8
          Top = 121
          Width = 234
          Height = 17
          Caption = 'Log'
          TabOrder = 4
        end
      end
    end
    object tsDictionary: TTntTabSheet
      Caption = 'Dictionary'
      object TntGroupBoxOnlineDictSearch: TTntGroupBox
        Left = 0
        Top = 0
        Width = 738
        Height = 193
        Align = alTop
        Caption = 'Online dictionary search'
        TabOrder = 0
        object chkOnLineSearchDict1: TCheckBox
          Left = 15
          Top = 67
          Width = 141
          Height = 20
          Caption = 'Include Word Reference'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 2
          OnClick = chkOnLineSearchDict1Click
        end
        object chkOnLineSearchDict2: TCheckBox
          Left = 15
          Top = 92
          Width = 171
          Height = 20
          Caption = 'Include WordNet'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 4
        end
        object chkOnLineSearchDict3: TCheckBox
          Left = 15
          Top = 116
          Width = 171
          Height = 20
          Caption = 'Include Urban Dictionary'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 5
        end
        object chkOnLineSearchDict4: TCheckBox
          Left = 15
          Top = 140
          Width = 171
          Height = 20
          Caption = 'Include OneLook'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 6
          Visible = False
        end
        object chkDictionaryOnLineEnabled: TCheckBox
          Left = 15
          Top = 17
          Width = 171
          Height = 17
          Caption = 'Enable online dictionary search'
          TabOrder = 0
          OnClick = chkDictionaryOnLineEnabledClick
        end
        object chkDictionaryOnLineExcludeTextPipe: TCheckBox
          Left = 15
          Top = 42
          Width = 171
          Height = 17
          Caption = 'Exclude search in text pipe'
          TabOrder = 1
        end
        object chkOnLineSearchDict5: TCheckBox
          Left = 15
          Top = 164
          Width = 171
          Height = 20
          Caption = 'Include TheFreeDictionary'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 7
          Visible = False
        end
        object cbbWordReferenceLanguage: TComboBox
          Left = 161
          Top = 67
          Width = 136
          Height = 21
          Style = csDropDownList
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Microsoft Sans Serif'
          Font.Style = []
          ItemHeight = 13
          ParentFont = False
          TabOrder = 3
          Items.Strings = (
            'ENIT (English-Italian)'
            'ENAR (English-Arabic)'
            'ENZH (English-Chinese)'
            'ENCZ (English-Czech)'
            'ENFR (English-French)'
            'ENGR (English-Greek)'
            'ENJA (English-Japanese)'
            'ENKO (English-Korean)'
            'ENPL (English-Polish)'
            'ENPT (English-Portuguese)'
            'ENRO (English-Romanian)'
            'ENES (English-Spanish)'
            'ENTR (English-Turkish)')
        end
      end
      object lbledtLinkDictionaries: TLabeledEdit
        Left = 2
        Top = 219
        Width = 210
        Height = 21
        EditLabel.Width = 136
        EditLabel.Height = 13
        EditLabel.Caption = 'Link to additional dictionaries'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 508
    Width = 746
    Height = 36
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object TntPanel1: TTntPanel
      Left = 584
      Top = 0
      Width = 162
      Height = 36
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      DesignSize = (
        162
        36)
      object bttOk: TTntButton
        Left = 1
        Top = 9
        Width = 75
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'OK'
        TabOrder = 0
        OnClick = bttOkClick
      end
      object bttCancel: TTntButton
        Left = 87
        Top = 9
        Width = 75
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Cancel'
        TabOrder = 1
        OnClick = bttCancelClick
      end
    end
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Microsoft Sans Serif'
    Font.Style = []
    Options = [fdAnsiOnly, fdEffects, fdNoVectorFonts, fdScalableOnly]
    Left = 532
    Top = 288
  end
  object pmErrorChecking: TPopupMenu
    Left = 679
    Top = 288
    object pmiSelectAll: TMenuItem
      Caption = 'Select all'
      OnClick = pmiSelectAllClick
    end
    object pmiUnselectAll: TMenuItem
      Caption = 'Unselect all'
      OnClick = pmiUnselectAllClick
    end
  end
  object ColorDialog1: TColorDialog
    Left = 601
    Top = 288
  end
  object TimerCheckCodec: TTimer
    Interval = 250
    OnTimer = TimerCheckCodecTimer
    Left = 454
    Top = 288
  end
  object TntRestoreConfigurationOpenDialog: TTntOpenDialog
    DefaultExt = '*.saved'
    Filter = '*.saved'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Title = 'Select a valid .saved file that contain configuration'
    Left = 544
    Top = 144
  end
end
