object Form3: TForm3
  Left = 0
  Top = 0
  Caption = #1042#1080#1088#1090#1091#1072#1083#1100#1085#1099#1081' '#1076#1085#1077#1074#1085#1080#1082
  ClientHeight = 700
  ClientWidth = 1122
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblNameOfMonths: TLabel
    Left = 408
    Top = 8
    Width = 142
    Height = 22
    Caption = 'lblNameOfMonths'
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Open Sans SemiBold'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblWeekDate: TLabel
    Left = 424
    Top = 36
    Width = 75
    Height = 18
    Caption = 'lblWeekDate'
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Open Sans'
    Font.Style = []
    ParentFont = False
  end
  object sgMonday: TStringGrid
    Left = 24
    Top = 60
    Width = 370
    Height = 203
    ColCount = 3
    DoubleBuffered = False
    FixedCols = 0
    RowCount = 8
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Open Sans'
    Font.Style = []
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSelect]
    ParentDoubleBuffered = False
    ParentFont = False
    ScrollBars = ssNone
    TabOrder = 0
  end
  object sgTuesday: TStringGrid
    Left = 24
    Top = 269
    Width = 370
    Height = 203
    ColCount = 3
    DoubleBuffered = False
    FixedCols = 0
    RowCount = 8
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Open Sans'
    Font.Style = []
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSelect]
    ParentDoubleBuffered = False
    ParentFont = False
    ScrollBars = ssNone
    TabOrder = 1
  end
  object sgWednesDay: TStringGrid
    Left = 24
    Top = 478
    Width = 370
    Height = 203
    ColCount = 3
    DoubleBuffered = False
    FixedCols = 0
    RowCount = 8
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Open Sans'
    Font.Style = []
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSelect]
    ParentDoubleBuffered = False
    ParentFont = False
    ScrollBars = ssNone
    TabOrder = 2
  end
  object sgThursDay: TStringGrid
    Left = 416
    Top = 60
    Width = 370
    Height = 203
    ColCount = 3
    DoubleBuffered = False
    FixedCols = 0
    RowCount = 8
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Open Sans'
    Font.Style = []
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSelect]
    ParentDoubleBuffered = False
    ParentFont = False
    ScrollBars = ssNone
    TabOrder = 3
  end
  object sgFriday: TStringGrid
    Left = 416
    Top = 269
    Width = 370
    Height = 203
    ColCount = 3
    DoubleBuffered = False
    FixedCols = 0
    RowCount = 8
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Open Sans'
    Font.Style = []
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSelect]
    ParentDoubleBuffered = False
    ParentFont = False
    ScrollBars = ssNone
    TabOrder = 4
  end
  object sgSaturday: TStringGrid
    Left = 416
    Top = 478
    Width = 370
    Height = 203
    ColCount = 3
    DoubleBuffered = False
    FixedCols = 0
    RowCount = 8
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Open Sans'
    Font.Style = []
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSelect]
    ParentDoubleBuffered = False
    ParentFont = False
    ScrollBars = ssNone
    TabOrder = 5
  end
  object pnlMainInfoBtns: TPanel
    Left = 816
    Top = 60
    Width = 281
    Height = 621
    TabOrder = 6
    object lblUserName: TLabel
      Left = 120
      Top = 16
      Width = 108
      Height = 20
      Caption = #1055#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1100
      Font.Charset = RUSSIAN_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Open Sans SemiBold'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object btnEditHW: TButton
      Left = 16
      Top = 568
      Width = 249
      Height = 41
      Caption = #1056#1077#1076#1072#1082#1090#1080#1088#1086#1074#1072#1090#1100' '#1044#1086#1084#1072#1096#1085#1080#1077' '#1079#1072#1076#1072#1085#1080#1103
      TabOrder = 0
      OnClick = btnEditHWClick
    end
    object btnEditUserList: TButton
      Left = 16
      Top = 440
      Width = 249
      Height = 41
      Caption = #1056#1077#1076#1072#1082#1090#1080#1088#1086#1074#1072#1090#1100' '#1089#1087#1080#1089#1086#1082' '#1087#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1077#1081
      TabOrder = 2
      OnClick = btnEditUserListClick
    end
    object btnEditMarks: TButton
      Left = 16
      Top = 487
      Width = 249
      Height = 41
      Caption = #1042#1099#1089#1090#1072#1074#1083#1077#1085#1080#1077' '#1086#1094#1077#1085#1086#1082
      TabOrder = 3
      Visible = False
      OnClick = btnEditMarksClick
    end
    object btnComplaints: TButton
      Left = 16
      Top = 392
      Width = 249
      Height = 42
      Caption = #1042#1099#1089#1090#1072#1074#1083#1077#1085#1080#1077' '#1079#1072#1084#1077#1095#1072#1085#1080#1081
      TabOrder = 1
      OnClick = btnComplaintsClick
    end
  end
  object btnPrevWeek: TButton
    Left = 25
    Top = 15
    Width = 75
    Height = 33
    Caption = #1053#1072#1079#1072#1076
    TabOrder = 7
    OnClick = btnPrevWeekClick
  end
  object btnNextWeek: TButton
    Left = 123
    Top = 15
    Width = 75
    Height = 33
    Caption = #1042#1087#1077#1088#1105#1076
    TabOrder = 8
    OnClick = btnNextWeekClick
  end
  object btnCurrentWeek: TButton
    Left = 680
    Top = 15
    Width = 106
    Height = 33
    Caption = #1058#1077#1082#1091#1097#1072#1103' '#1085#1077#1076#1077#1083#1103
    TabOrder = 9
    OnClick = btnCurrentWeekClick
  end
end
