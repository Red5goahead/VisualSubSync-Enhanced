object AboutForm: TAboutForm
  Left = 384
  Top = 180
  AutoSize = True
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  BorderWidth = 4
  Caption = 'About...'
  ClientHeight = 426
  ClientWidth = 345
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = [fsBold]
  KeyPreview = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 14
  object TntLabel1: TTntLabel
    Left = 0
    Top = 0
    Width = 345
    Height = 33
    Cursor = crHandPoint
    Alignment = taCenter
    AutoSize = False
    Caption = 'VisualSubSync'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -24
    Font.Name = 'Arial'
    Font.Style = [fsBold, fsUnderline]
    ParentFont = False
    OnClick = TntLabel1Click
  end
  object Bevel1: TBevel
    Left = 0
    Top = 32
    Width = 345
    Height = 9
    Shape = bsBottomLine
  end
  object TntLabel2: TTntLabel
    Left = 8
    Top = 72
    Width = 109
    Height = 14
    Caption = 'Copyright 2003-2017'
  end
  object TntLabel3: TTntLabel
    Left = 8
    Top = 88
    Width = 137
    Height = 14
    Cursor = crHandPoint
    Caption = 'christophe.paris@free.fr'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold, fsUnderline]
    ParentFont = False
    OnClick = TntLabel3Click
  end
  object LabelVersion: TTntLabel
    Left = 8
    Top = 48
    Width = 97
    Height = 19
    Caption = 'Version 0.0.0'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Bevel2: TBevel
    Left = 0
    Top = 387
    Width = 345
    Height = 10
    Shape = bsBottomLine
  end
  object TntLabel4: TTntLabel
    Left = 8
    Top = 130
    Width = 289
    Height = 14
    AutoSize = False
    Caption = 'Thanks to the authors of the following components :'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object lblVTV: TTntLabel
    Left = 8
    Top = 146
    Width = 217
    Height = 14
    AutoSize = False
    Caption = '- Virtual Treeview by Mike Lischke'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object TntLabel6: TTntLabel
    Left = 8
    Top = 162
    Width = 217
    Height = 14
    AutoSize = False
    Caption = '- Tnt Unicode Controls by Troy Wolbrink'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object lblJS: TTntLabel
    Left = 8
    Top = 178
    Width = 273
    Height = 14
    AutoSize = False
    Caption = '- JavaScript Bridge / SpiderMonkey JavaScript Engine'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object TntLabel8: TTntLabel
    Left = 8
    Top = 194
    Width = 273
    Height = 14
    AutoSize = False
    Caption = '- VSFilter by Gabest'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object TntLabel5: TTntLabel
    Left = 8
    Top = 210
    Width = 273
    Height = 14
    AutoSize = False
    Caption = '- Hunspell by L'#225'szl'#243' N'#233'meth'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object TntLabel9: TTntLabel
    Left = 245
    Top = 72
    Width = 93
    Height = 14
    Alignment = taRightJustify
    Caption = 'by Kickaha, Nixxo'
  end
  object TntLabel11: TTntLabel
    Left = 224
    Top = 105
    Width = 114
    Height = 14
    Cursor = crHandPoint
    Caption = 'www.italiansubs.net'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold, fsUnderline]
    ParentFont = False
    OnClick = TntLabel1Click
  end
  object TntLabel10: TTntLabel
    Left = 7
    Top = 226
    Width = 273
    Height = 14
    AutoSize = False
    Caption = '- Matroska (http://www.matroska.org)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object TntLabel12: TTntLabel
    Left = 7
    Top = 242
    Width = 322
    Height = 14
    AutoSize = False
    Caption = '- Mpc Home Cinema filters (mpc-hc.sourceforge.net)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object TntLabel13: TTntLabel
    Left = 7
    Top = 258
    Width = 287
    Height = 14
    AutoSize = False
    Caption = '- MadFlac filter by Madshi (http://www.madshi.net/)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object TntLabel14: TTntLabel
    Left = 7
    Top = 273
    Width = 322
    Height = 14
    AutoSize = False
    Caption = 
      '- Lav Filter 0.69 (https://github.com/Nevcairiel/LAVFilters/rele' +
      'ases)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object TntLabel15: TTntLabel
    Left = 7
    Top = 289
    Width = 287
    Height = 14
    AutoSize = False
    Caption = '- Ararat Synapse (http://www.ararat.cz/synapse/doku.php/start)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object TntLabel16: TTntLabel
    Left = 7
    Top = 305
    Width = 287
    Height = 14
    AutoSize = False
    Caption = '- KAZip (http://kadao.dir.bg/)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object TntLabel17: TTntLabel
    Left = 221
    Top = 87
    Width = 118
    Height = 14
    Alignment = taRightJustify
    Caption = 'Red5goahead, Rock3r'
  end
  object Label19: TTntLabel
    Left = 7
    Top = 322
    Width = 287
    Height = 14
    AutoSize = False
    Caption = '- MP4Box (http://gpac.wp.mines-telecom.fr/mp4box/)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object TntLabel18: TTntLabel
    Left = 6
    Top = 339
    Width = 287
    Height = 14
    AutoSize = False
    Caption = '- MediaInfo (http://mediainfo.sourceforge.net/it)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object TntLabel19: TTntLabel
    Left = 6
    Top = 356
    Width = 287
    Height = 14
    AutoSize = False
    Caption = '- FFMpeg (http://ffmpeg.zeranoe.com)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object TntLabel7: TTntLabel
    Left = 6
    Top = 372
    Width = 287
    Height = 14
    AutoSize = False
    Caption = '- Subtitle Edit (http://www.nikse.dk/subtitleedit)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object bttOk: TTntButton
    Left = 8
    Top = 401
    Width = 329
    Height = 25
    Caption = 'OK'
    TabOrder = 0
    OnClick = bttOkClick
  end
end
