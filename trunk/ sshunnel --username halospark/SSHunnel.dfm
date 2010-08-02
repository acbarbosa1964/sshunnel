object Form6: TForm6
  Left = 0
  Top = 0
  AlphaBlendValue = 230
  BorderIcons = []
  BorderStyle = bsToolWindow
  Caption = 'SSHunnel'
  ClientHeight = 187
  ClientWidth = 282
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clNavy
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 0
    Width = 265
    Height = 49
    Caption = 'Server'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentBackground = False
    ParentColor = False
    ParentFont = False
    TabOrder = 0
    object Label1: TLabel
      Left = 179
      Top = 19
      Width = 3
      Height = 13
      Caption = ':'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Edit1: TEdit
      Left = 16
      Top = 16
      Width = 161
      Height = 21
      TabOrder = 0
      Text = '127.0.0.1'
    end
    object Edit2: TEdit
      Left = 184
      Top = 16
      Width = 64
      Height = 21
      TabOrder = 1
      Text = '22'
    end
  end
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 55
    Width = 105
    Height = 46
    Caption = 'SSH Tunnel Port'
    TabOrder = 1
  end
  object Edit3: TEdit
    Left = 24
    Top = 72
    Width = 65
    Height = 21
    TabOrder = 2
    Text = '1337'
  end
  object GroupBox2: TGroupBox
    Left = 119
    Top = 55
    Width = 154
    Height = 46
    Caption = 'User'
    TabOrder = 3
    object Edit4: TEdit
      Left = 16
      Top = 16
      Width = 121
      Height = 21
      TabOrder = 0
      Text = 'root'
    end
  end
  object RadioGroup2: TRadioGroup
    Left = 8
    Top = 104
    Width = 265
    Height = 49
    Caption = 'Password'
    TabOrder = 4
  end
  object Edit5: TEdit
    Left = 24
    Top = 120
    Width = 232
    Height = 21
    PasswordChar = '*'
    TabOrder = 5
  end
  object Button1: TButton
    Left = 8
    Top = 159
    Width = 266
    Height = 23
    Caption = 'Save'
    TabOrder = 6
    OnClick = Button1Click
  end
  object PopupMenu1: TPopupMenu
    BiDiMode = bdLeftToRight
    ParentBiDiMode = False
    Left = 24
    Top = 248
    object pcon: TMenuItem
      Caption = 'Connect'
      OnClick = pconClick
    end
    object pdiscon: TMenuItem
      Caption = 'Disconnect'
      Enabled = False
      OnClick = pdisconClick
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object psettings: TMenuItem
      Caption = 'Settings'
      OnClick = psettingsClick
    end
    object N1: TMenuItem
      Caption = '-'
      Enabled = False
    end
    object pExit: TMenuItem
      Caption = 'Exit'
      OnClick = pExitClick
    end
  end
end
