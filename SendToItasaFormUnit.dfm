object SendToItasaForm: TSendToItasaForm
  Left = 379
  Top = 130
  BorderStyle = bsToolWindow
  Caption = 'Send Resynch to ItaSA'
  ClientHeight = 483
  ClientWidth = 352
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Microsoft Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Shape1: TTntShape
    Left = 14
    Top = 30
    Width = 322
    Height = 23
  end
  object TntLabel1: TTntLabel
    Left = 16
    Top = 8
    Width = 271
    Height = 13
    Caption = 'Add the version to the filename. (ex. 720p, WEB-DL, etc.)'
  end
  object ButtonPacket: TTntSpeedButton
    Left = 311
    Top = 3
    Width = 23
    Height = 22
    Hint = 'Create Complete Season Packet'
    Glyph.Data = {
      36030000424D3603000000000000360000002800000010000000100000000100
      1800000000000003000000000000000000000000000000000000FF00FFFF00FF
      FF00FFFF00FFCFCFCFCBCBCBCBCBCBCBCBCBCBCBCBCBCBCBCBCBCBCBCBCBCBCB
      CBCBCBCBCBCBCBCECECEFF00FFFF00FFFF00FFFF00FFB88826B67E0EB47B09B4
      7A08B47B08B47B08B47B08B47B08B47B08B47B09B67E0EB88623FF00FFFF00FF
      DCDCDCCBCBCBB67E0EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFB67E0EFF00FFFF00FFBE9A53B88114B37905FFFFFFFFFFFFFF
      FFFFD7C0A5FFFFFFD8C0A6FFFFFFD7BFA5FFFFFFFFFFFFB47B09CFCFCFCBCBCB
      B88114FFFFFFB17500FFFFFFD5BA9BD6BC9ED8BFA2D8BFA2D9C0A3D8BFA2D8BF
      A1D6BC9DFFFFFFB47B08B88826B78012B57C0BFFFFFFB07300FFFFFFFFFFF5FF
      FFF9D9BE9DFFFFFDD9BE9DFFFFFDD8BD9CFFFFF8FFFFFFB47B08B67F11FFFFFF
      B27702FFFFFFB07300FFFFFFD5B691D7B995D9BC99D9BC99D9BD99D9BC99D9BC
      98D7B893FFFFFFB47B09B57D0CFFFFFFB17500FFFFFFB07300FFFFFFFFFAE2FF
      FDE7D9BA92FFFFEBD9BA93FFFFEBD8B991FFFCE5FFFFFFB47B08B57C0BFFFFFF
      B07400FFFFFFB07300FFFFFFDDB280DFB685DEB78ADEB78ADEB88BDEB78ADDB7
      89DBB383FFFFFFB57B08B57C0BFFFFFFB07400FFFFFFB17300FFFFFF44C4FF46
      C6FFE2B57F43C7FFE1B68043C7FFE1B57E40C4FFFFFFFFB57C09B57C0BFFFFFF
      B07400FFFFFFB27300FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFB67E0EB57C0BFFFFFFB07200FFFFFF8E8E54B17100B07100B0
      7100AF7100AF7100AF7100B07100B17400B47903B67E0DBB8A27B57C0AFFFFFF
      B17200FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFB77F11FF00FFFF00FFB57C0AFFFFFF869265B17100AF7100AF7100AF7200AF
      7200AF7200AF7200B17400B37A06B67F10CDA960FF00FFFF00FFB67E0FFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB77F11FF00
      FFFF00FFFF00FFFF00FFBB8A26B67E0FB57C0AB57C0AB57C0BB57C0BB57C0BB5
      7C0BB57C0BB57D0CB77F11BB8A27FF00FFFF00FFFF00FFFF00FF}
    OnClick = ButtonPacketClick
  end
  object Shape2: TTntShape
    Left = 125
    Top = 84
    Width = 65
    Height = 23
    Visible = False
  end
  object EditRename: TTntEdit
    Left = 15
    Top = 31
    Width = 320
    Height = 21
    TabOrder = 0
    OnChange = EditRenameChange
  end
  object ButtonSend: TTntButton
    Left = 260
    Top = 275
    Width = 70
    Height = 25
    Caption = 'Send'
    Enabled = False
    TabOrder = 8
    OnClick = ButtonSendClick
  end
  object ButtonAddResynch: TTntButton
    Left = 230
    Top = 64
    Width = 100
    Height = 25
    Caption = 'Add To Queue'
    TabOrder = 5
    OnClick = ButtonAddResynchClick
  end
  object vdtQueue: TVirtualDrawTree
    Left = 10
    Top = 120
    Width = 330
    Height = 145
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.MainColumn = -1
    PopupMenu = TntPopupMenu1
    TabOrder = 6
    OnCompareNodes = vdtQueueCompareNodes
    OnDrawNode = vdtQueueDrawNode
    Columns = <>
  end
  object rbPannello: TTntRadioButton
    Left = 24
    Top = 64
    Width = 91
    Height = 17
    Caption = 'Pannello ItaSA'
    TabOrder = 1
    OnClick = rbPannelloClick
  end
  object rbTopic: TTntRadioButton
    Left = 128
    Top = 64
    Width = 73
    Height = 17
    Caption = 'Topic'
    TabOrder = 2
    OnClick = rbTopicClick
  end
  object MemoPreviewMsg: TTntMemo
    Left = 20
    Top = 320
    Width = 310
    Height = 105
    Lines.Strings = (
      'MemoPreviewMsg')
    TabOrder = 9
  end
  object ButtonMsgOk: TTntButton
    Left = 260
    Top = 440
    Width = 70
    Height = 25
    Caption = 'Send'
    Enabled = False
    TabOrder = 13
    OnClick = ButtonMsgOkClick
  end
  object ChkPreviewMsg: TTntCheckBox
    Left = 20
    Top = 275
    Width = 137
    Height = 17
    Caption = 'Preview Topic Message'
    Checked = True
    Enabled = False
    State = cbChecked
    TabOrder = 7
  end
  object ButtonBold: TTntButton
    Left = 35
    Top = 432
    Width = 21
    Height = 21
    Caption = 'B'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Microsoft Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 10
    OnClick = ButtonBoldClick
  end
  object ButtonItalic: TTntButton
    Left = 66
    Top = 432
    Width = 21
    Height = 21
    Caption = 'I'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Microsoft Sans Serif'
    Font.Style = [fsItalic]
    ParentFont = False
    TabOrder = 11
    OnClick = ButtonItalicClick
  end
  object ButtonUnderline: TTntButton
    Left = 97
    Top = 432
    Width = 21
    Height = 21
    Caption = 'U'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Microsoft Sans Serif'
    Font.Style = [fsUnderline]
    ParentFont = False
    TabOrder = 12
    OnClick = ButtonUnderlineClick
  end
  object rbManual: TTntRadioButton
    Left = 24
    Top = 88
    Width = 98
    Height = 17
    Caption = 'Manual Upload'
    TabOrder = 3
    OnClick = rbManualClick
  end
  object EditTopicID: TTntEdit
    Left = 126
    Top = 85
    Width = 63
    Height = 21
    TabOrder = 4
    Visible = False
    OnChange = EditTopicIDChange
  end
  object TntPopupMenu1: TTntPopupMenu
    Left = 56
    Top = 144
    object Removeselected1: TTntMenuItem
      Caption = 'Remove selected'
      OnClick = Removeselected1Click
    end
  end
  object OpenDialog1: TTntOpenDialog
    Left = 336
  end
end
