object frmAuthorization: TfrmAuthorization
  Left = 350
  Top = 350
  Caption = #1040#1074#1090#1086#1088#1080#1079#1072#1094#1080#1103
  ClientHeight = 149
  ClientWidth = 309
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object lblLogin: TLabel
    Left = 48
    Top = 25
    Width = 34
    Height = 13
    Caption = #1051#1086#1075#1080#1085':'
  end
  object lblPassWord: TLabel
    Left = 48
    Top = 72
    Width = 41
    Height = 13
    Caption = #1055#1072#1088#1086#1083#1100':'
  end
  object lblWrong: TLabel
    Left = 64
    Top = 3
    Width = 3
    Height = 13
  end
  object edLogin: TEdit
    Left = 96
    Top = 22
    Width = 153
    Height = 21
    TabOrder = 0
    OnClick = edLoginClick
  end
  object edPassWord: TEdit
    Left = 95
    Top = 64
    Width = 153
    Height = 21
    TabOrder = 1
    OnClick = edLoginClick
  end
  object btnEnter: TBitBtn
    Left = 48
    Top = 100
    Width = 89
    Height = 36
    Caption = #1051#1086#1075#1080#1085
    TabOrder = 2
    OnClick = btnEnterClick
  end
  object btnExit: TBitBtn
    Left = 160
    Top = 100
    Width = 89
    Height = 36
    Caption = #1042#1099#1093#1086#1076
    TabOrder = 3
    OnClick = btnExitClick
  end
end
