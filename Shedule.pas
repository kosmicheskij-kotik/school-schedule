unit Shedule;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    lblNameOfMonths: TLabel;
    lblWeekDate: TLabel;
    sgMonday: TStringGrid;
    sgTuesday: TStringGrid;
    sgWednesDay: TStringGrid;
    sgThursDay: TStringGrid;
    sgFriday: TStringGrid;
    sgSaturday: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure LoadSheduletosg(Sender: TObject; DateOfMonday: TDateTime);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  function getSubjectNameByDate(SubjDate: TDateTime; NumOfSubject: Integer):string;
  function getDayNameByDate(DayDate: TDateTime): string;

var
  Form1: TForm1;

implementation

uses
  System.DateUtils, HomeWorkEditing;

type
  PSubjectList = ^TSubjectList;
  TSubjects = record
    ID: integer;
    Name: String[25];
  end;
  TSubjectList = record
    Subject: TSubjects;
    NextSubject: PSubjectList;
  end;

  TShedule = array[1..6] of array [1..7] of integer;

  TDayOfWeek = record
    Monday, TuesDay,
    Wednesday, ThursDay,
    Friday, SaturDay: string;
  end;

const
  CHomeWork: string = 'Домашнее задание';
  CMark: string = 'Отметка';
  CDayOfWeek : TDayOfWeek = (
    Monday: 'Понедельник';
    TuesDay:  'Вторник';
    WednesDay:  'Среда';
    ThursDay:  'Четверг';
    Friday:  'Пятница';
    SaturDay:  'Суббота');
  MsgIsWeekend: string = 'Выбранный день - выходной. Пожалуйста, выберите другую дату';

var
  SheduleList: TShedule;
  FirstSubject: PSubjectList;

function getSubjectbyId(id: integer):string; forward;

function getSubjectNameByDate(SubjDate: TDateTime; NumOfSubject: Integer):string;
var
  DayOfWeekValue: Integer;
begin
  DayOfWeekValue := DayOfWeek(SubjDate) - 1;
  if DayOfWeekValue <> 0 then
    Result := getSubjectbyId(SheduleList[DayOfWeekValue, NumOfSubject])
  else
    Result := 'Weekend';
end;

// Процедура добавления нового предмета в конец списка
procedure addSubject(var Point: PSubjectList; const Name: string; id: integer);
begin
  New(Point^.NextSubject);
  Point := Point^.NextSubject;
  Point^.Subject.Name := name;
  Point^.Subject.id := id;
  Point^.NextSubject := nil;
end;

// Загрузка всех существующих предметов из файла
function loadSubjectsFromFile(var FirstSubject:PSubjectList): integer;
var
  SubjectsFile: File of TSubjects;
  CurrentSubject: PSubjectList;
  NewSubject: TSubjects;
begin
  if FileExists('Subjects.dat') then
  begin
    AssignFile(SubjectsFile, 'Subjects.dat');
    Reset(SubjectsFile);
    CurrentSubject := FirstSubject;
    CurrentSubject^.NextSubject := nil;
    Result := 0;
    While not EoF(SubjectsFile) do
    begin
      read(SubjectsFile, NewSubject);
      addSubject(CurrentSubject, NewSubject.Name, NewSubject.id);
      inc(Result);
    end;
    CloseFile(SubjectsFile);
  end
  else
    Result := 0;
end;

procedure saveSubjectsToFile(FirstSubject:PSubjectList);
var
  SubjectsFile: File of TSubjects;
  CurrentSubject: PSubjectList;
begin
  AssignFile(SubjectsFile, 'Subjects.dat');
  Rewrite(SubjectsFile);
  CurrentSubject := FirstSubject;
  While CurrentSubject^.NextSubject <> nil do
  begin
    CurrentSubject := CurrentSubject^.NextSubject;
    write(SubjectsFile, CurrentSubject^.Subject);
  end;
  CloseFile(SubjectsFile);
end;

function getSubjectID(Subject: string; FirstSubject: PSubjectList):integer;
var
  SubjectsFile: File of TSubjects;
  CurrentSubject: PSubjectList;
  isSubjectFound: boolean;
  prevSubjectId: integer;
begin

  CurrentSubject := FirstSubject;
  isSubjectFound := false;

  While (CurrentSubject^.NextSubject <> Nil) and (not isSubjectFound) do
  begin
    CurrentSubject := CurrentSubject^.NextSubject;
    if CurrentSubject^.Subject.Name = Subject then
    begin
      Result := CurrentSubject^.Subject.ID;
      isSubjectFound := true;
    end;
  end;
  if not isSubjectFound then
  begin
    prevSubjectId := CurrentSubject^.Subject.Id;
    addSubject(CurrentSubject, Subject, prevSubjectId+1);
    Result := CurrentSubject^.Subject.id;
  end;
  saveSubjectsToFile(FirstSubject);
end;

function getSubjectbyId(id: integer):string;
var
  CurrentSubject: PSubjectList;
  isSubjectFound: boolean;
begin
  CurrentSubject := FirstSubject;
  isSubjectFound := false;
  While (CurrentSubject^.NextSubject <> Nil) and (not isSubjectFound) do
  begin
    CurrentSubject := CurrentSubject^.NextSubject;
    if CurrentSubject^.Subject.Id = id then
    begin
      Result := CurrentSubject^.Subject.Name;
      isSubjectFound := true;
    end;
  end;
  if not isSubjectFound then
    Result := '';
end; 

function getDayNameByDate(DayDate: TDateTime): string;
var
  DayValue : integer;
  MyYear, MyMonths, MyDay: Word;
begin
  DecodeDate(DayDate, MyYear, MyMonths, MyDay);
  DayValue := DayOfWeek(DayDate) - 1;
  case DayValue of
    1: Result := CDayOfWeek.Monday;
    2: Result := CDayOfWeek.TuesDay;
    3: Result := CDayOfWeek.Wednesday;
    4: Result := CDayOfWeek.ThursDay;
    5: Result := CDayOfWeek.Friday;
    6: Result := CDayOfWeek.SaturDay;
  else Result := 'Weekend';
  end;
  Result := Result + ', ' + IntToStr(MyDay);
end;

(* Процедура загрузки расписания из текстового файла
** в типизированный файл
*)
procedure LoadSheduleFromTextFile(var FirstSubject: PSubjectList);
Var
  SheduleTextFile : TextFile;
  SheduleFile: File of TShedule;
  dayOfWeek, NumOfSubject: integer;
  Subject: string;
  SubjectID: integer;
  CurrentSubject: PSubjectList;
  SubjectsLoaded: integer;

begin
  CurrentSubject := FirstSubject;
  CurrentSubject^.NextSubject := nil;
  SubjectsLoaded := loadSubjectsFromFile(FirstSubject);
  addSubject(CurrentSubject, '', 0);
  
  AssignFile(SheduleTextFile, 'Shedule.txt');
  AssignFile(SheduleFile, 'Shedule.dat');
  Reset(SheduleTextFile);
  Rewrite(SheduleFile);

  dayOfWeek := 1;
  NumOfSubject := 0;
  While not EoF(SheduleTextFile) do
  begin
    readln(SheduleTextFile, Subject);
    if Subject <> '' then
    begin
      SubjectID := getSubjectID(Subject, FirstSubject);
      inc(NumOfSubject);
      if NumOfSubject <= 8 then
        SheduleList[dayOfWeek, NumOfSubject] := SubjectID;
    end
    else
    begin
      inc(dayOfWeek);
      NumOfSubject := 0;
    end;
  end;

  write(SheduleFile, SheduleList);
  CloseFile(SheduleTextFile);
  CloseFile(SheduleFile);
end;

procedure LoadSheduleFromFile();
var
  SheduleFile : File of TShedule;
begin
  AssignFile(SheduleFile, 'Shedule.dat');
  Reset(SheduleFile);
  Read(SheduleFile, SheduleList);
  CloseFile(SheduleFile);
end;

procedure TForm1.LoadSheduletosg(Sender: TObject; DateOfMonday: TDateTime);
var
  i : integer;
  Subject: string;
  MyYear, MyMonths, MyPrevMonths, MyDay: Word;
  DateOfDay: TDateTime;

  function getMonthsName(Months:Integer):String;
  begin
    case Months of
      1: Result := 'Январь';
      2: Result := 'Февраль';
      3: Result := 'Март';
      4: Result := 'Апрель';
      5: Result := 'Май';
      6: Result := 'Июнь';
      7: Result := 'Июль';
      8: Result := 'Август';
      9: Result := 'Сентябрь';
      10: Result := 'Октябрь';
      11: Result := 'Ноябрь';
      12: Result := 'Декабрь';
    end;
  end;

begin
  DateOfDay := DateOfMonday;
  DecodeDate(DateOfDay, MyYear, MyPrevMonths, MyDay);
  with sgMonday do
  begin
    Cells[0,0] := getDayNameByDate(DateOfDay);
    Cells[1,0] := CHomeWork;
    Cells[2,0] := CMark;
    ColWidths[0]:= 104;
    ColWidths[1] := 196;
    ColWidths[2] := 64;
    for i := 1 to 7 do
    begin
      Subject := getSubjectbyId(SheduleList[1, i]);
      Cells[0,i] := IntToStr(i) + '. ' + Subject;
    end;
  end;
  DateOfDay := IncDay(DateOfDay, 1);
  with sgTuesday do
  begin
    Cells[0,0] := getDayNameByDate(DateOfDay);
    Cells[1,0] := CHomeWork;
    Cells[2,0] := CMark;
    ColWidths[0]:= 104;
    ColWidths[1] := 196;
    ColWidths[2] := 64;
    for i := 1 to 7 do
    begin
      Cells[0,i] := IntToStr(i) + '. ' + getSubjectbyId(SheduleList[2, i]);
    end;
  end;
  DateOfDay := IncDay(DateOfDay, 1);
  with sgWednesDay do
  begin
    Cells[0,0] := getDayNameByDate(DateOfDay);
    Cells[1,0] := CHomeWork;
    Cells[2,0] := CMark;
    ColWidths[0]:= 104;
    ColWidths[1] := 196;
    ColWidths[2] := 64;
    for i := 1 to 7 do
    begin
      Cells[0,i] := IntToStr(i) + '. ' + getSubjectbyId(SheduleList[3, i]);
    end;
  end;
  DateOfDay := IncDay(DateOfDay, 1);
  with sgThursDay do
  begin
    Cells[0,0] := getDayNameByDate(DateOfDay);
    Cells[1,0] := CHomeWork;
    Cells[2,0] := CMark;
    ColWidths[0]:= 104;
    ColWidths[1] := 196;
    ColWidths[2] := 64;
    for i := 1 to 7 do
    begin
      Cells[0,i] := IntToStr(i) + '. ' + getSubjectbyId(SheduleList[4, i]);
    end;
  end;
  DateOfDay := IncDay(DateOfDay, 1);
  with sgFriday do
  begin
    Cells[0,0] := getDayNameByDate(DateOfDay);
    Cells[1,0] := CHomeWork;
    Cells[2,0] := CMark;
    ColWidths[0]:= 104;
    ColWidths[1] := 196;
    ColWidths[2] := 64;
    for i := 1 to 7 do
    begin
      Cells[0,i] := IntToStr(i) + '. ' + getSubjectbyId(SheduleList[5, i]);
    end;
  end;
  DateOfDay := IncDay(DateOfDay, 1);
  DecodeDate(DateOfDay, MyYear, MyMonths, MyDay);
  with sgSaturday do
  begin
    Cells[0,0] := getDayNameByDate(DateOfDay);
    Cells[1,0] := CHomeWork;
    Cells[2,0] := CMark;
    ColWidths[0]:= 104;
    ColWidths[1] := 196;
    ColWidths[2] := 64;
    for i := 1 to 7 do
    begin
      Cells[0,i] := IntToStr(i) + '. ' + getSubjectbyId(SheduleList[6, i]);
    end;
  end;
  lblWeekDate.Caption := DateToStr(DateOfMonday) + ' - ' + DateToStr(DateOfDay);
  lblWeekDate.Left := Trunc(ClientWidth * 0.5) - lblWeekDate.Width div 2;
  if (MyMonths - MyPrevMonths = 0) then
    lblNameOfMonths.Caption := getMonthsName(MyMonths)
  else
    lblNameOfMonths.Caption := getMonthsName(MyPrevMonths) + ' / ' + getMonthsName(MyMonths);
  lblNameOfMonths.Left := Trunc(ClientWidth * 0.5) - lblNameOfMonths.Width div 2;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  DayOfMonday: TDateTime;
  CurrentDayOfWeek: Integer;
begin
  New(FirstSubject);
  CurrentDayOfWeek := DayOfWeek(Date);
  DayOfMonday := Date;
  if (CurrentDayOfWeek >= 2) then
    DayOfMonday := IncDay(DayOfMonday, 2-CurrentDayOfWeek)
  else
    DayOfMonday := IncDay(DayOfMonday, 1);
  LoadSheduleFromTextFile(FirstSubject);
  LoadSheduletosg(Sender, DayOfMonday);
end;

end.
