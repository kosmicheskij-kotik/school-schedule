unit HomeWorkEditing;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls,
  Vcl.Grids;

type
  TForm2 = class(TForm)
    lblChoosedate: TLabel;
    sgDayShedule: TStringGrid;
    btnSaveHW: TButton;
    lblShedule: TLabel;
    dtpHWEditing: TDateTimePicker;
    procedure FormCreate(Sender: TObject);
    procedure dtpHWEditingChange(Sender: TObject);
    procedure btnSaveHWClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;
  PHomeWork = ^THomeWorkList;
  THomeWork = record
    Date: TDateTime;
    NumOfSubject: Integer;
    Value: string[40];
  end;
  THomeWorkList = record
    HomeWork: THomeWork;
    NextHW: PHomeWork;
  end;
var
  Form2: TForm2;
  FirstHW: PHomeWork;
  LastHW: PHomeWork;

 function GetHWByDate(HWDate: TDateTime; NumOfSubject: integer; var FirstHW: PHomeWork): string;
 procedure LoadHWFromFile(var FirstHW, LastHW: PHomeWork);

implementation
{$R *.dfm}
uses
  MainShedule;

var
  CurrentDate : TDateTime;

procedure DisposeEl(LastHW: PHomeWork);
var
  temp : PHomeWork;
begin
  temp := LastHW;
  if LastHW^.NextHW <> nil then
    LastHW^.NextHW := LastHW^.NextHW.NextHW;
  Dispose(temp);
end;

procedure AddHW(HWDate: TDateTime; NumOfSubject: integer; Value: string;
                var LastHW: PHomeWork);
begin
  New(LastHW^.NextHW);
  LastHW := LastHW^.NextHW;
  LastHW^.HomeWork.Date := HWDate;
  LastHW^.HomeWork.NumOfSubject := NumOfSubject;
  LastHW^.HomeWork.Value := Value;
  LastHW^.NextHW := nil;
end;

function GetHWByDate(HWDate: TDateTime; NumOfSubject: integer; var FirstHW: PHomeWork): string;
var
  LastHW: PHomeWork;
  isHWFound : boolean;
begin
  LastHW := FirstHW;
  isHWFound := false;
  while (LastHW^.NextHW <> nil) and (not isHWFound) do
  begin
    LastHW := LastHW^.NextHW;
    if (LastHW^.HomeWork.Date = HWDate) and 
       (LastHW^.HomeWork.NumOfSubject = NumOfSubject) then
    begin
      isHWFound := true;
      Result := LastHW^.HomeWork.Value;
    end;

  end;
  if Not isHWFound then
    Result := '';
end;

procedure EditHWByDate(HWDate: TDateTime; NumOfSubject: integer; Value: String; var FirstHW: PHomeWork);
var
  LastHW: PHomeWork;
  isHWFound: boolean;
begin
  LastHW := FirstHW;
  isHWFound := false;
  while (LastHW^.NextHW <> nil) and (not isHWFound) do
  begin
    LastHW := LastHW^.NextHW;
    if (LastHW^.HomeWork.Date = HWDate) and 
       (LastHW^.HomeWork.NumOfSubject = NumOfSubject) then
    begin
      isHWFound := true;
      if Length(Value) <> 0 then
        LastHW^.HomeWork.Value := Value
      else
        DisposeEl(LastHW);
    end;
  end;
end;

procedure LoadHWFromFile(var FirstHW, LastHW: PHomeWork);
var
  HomeWorkFile: File of THomeWork;
  NewHW: THomeWork;
begin
  New(FirstHW);
  LastHW := FirstHW;
  LastHW^.NextHW := nil;

  if FileExists('HomeWork.dat') then
  begin
    AssignFile(HomeWorkFile, 'HomeWork.dat');
    Reset(HomeWorkFile);
    while not EoF(HomeWorkFile) do
    begin
      read(HomeWorkFile, NewHW);
      AddHW(NewHW.Date, NewHW.NumOfSubject, NewHW.Value, LastHW);
    end;
    CloseFile(HomeWorkFile);
  end;
end;

procedure SaveHWToFile(var FirstHW: PHomeWork);
var
  HomeWorkFile: File of THomeWork;
  CurrentHW : PHomeWork;
begin
  CurrentHW := FirstHW;
  AssignFile(HomeWorkFile, 'HomeWork.dat');
  Rewrite(HomeWorkFile);
  while CurrentHW^.NextHW <> nil do
  begin
    CurrentHW := CurrentHW^.NextHW;
    if Length(CurrentHW^.HomeWork.Value) <> 0 then
      write(HomeWorkFile, CurrentHW^.HomeWork)
    else
      disposeEl(CurrentHW);
  end;
  CloseFile(HomeWorkFile);
end;

procedure TForm2.btnSaveHWClick(Sender: TObject);
var
  HomeWorkValue: string;
  Row: integer;
  TestHW: String;
  DayOfMonday: TDateTime;
begin
  for Row := 1 to 7 do
  begin
    HomeWorkValue := GetHWByDate(CurrentDate, Row, FirstHW);
    if Length(HomeWorkValue) <> 0 then
      EditHWByDate(CurrentDate, Row, sgDayShedule.Cells[1,Row], FirstHW)
    else
    begin
      TestHW := sgDayShedule.Cells[1, Row];
      AddHW(CurrentDate, Row, sgDayShedule.Cells[1,Row], LastHW);
    end;
  end;
  SaveHWToFile(FirstHW);
  DayOfMonday := getDateOfMonday(Date);
  Form3.LoadSheduletosg(Sender, DayOfMonday);
  ShowMessage('Успешно сохранено!');
end;

procedure TForm2.dtpHWEditingChange(Sender: TObject);
var 
  DayOfWeekValue: integer;
  i: integer;
begin
  DayOfWeekValue := DayOfWeek(dtpHWEditing.Date) - 1;
  if DayOfWeekValue <> 0 then
    with sgDayShedule do
    begin
      Cells[0,0] := getDayNameByDate(dtpHWEditing.Date);
      for i := 1 to 7 do
      begin
        Cells[0,i] := InttoStr(i) + '. ' + getSubjectNameByDate(dtpHWEditing.Date, i);
        Cells[1,i] := GetHWByDate(dtpHWEditing.Date, i, FirstHW);
      end;
    end
  else
  begin
    ShowMessage(MsgIsWeekend);
  end;
  CurrentDate := dtpHWEditing.Date;
end;

procedure TForm2.FormActivate(Sender: TObject);
begin
  dtpHWEditingChange(Sender);
end;

procedure TForm2.FormCreate(Sender: TObject);
var 
  DayOfWeekValue: integer;
begin
  DayOfWeekValue := DayOfWeek(Date) - 1;
  CurrentDate := Date;
  with sgDayShedule do
  begin
    ColWidths[0] := 104;
    ColWidths[1] := 260;
    Cells[1,0] := CHomeWork;
  end;
  dtpHWEditing.Date := Date;
end;

end.

