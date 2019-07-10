unit UserComplaints;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls;

type
  PComplaints = ^TComplaintsList;
  TComplaint = record
    Date: TDateTime;
    Username: String[25];
    Value: ShortString;
  end;
  TComplaintsList = record
    Complaint: TComplaint;
    NextComp: PComplaints;
  end;
  TfrmUserComplaints = class(TForm)
    cbbStudents: TComboBox;
    edtAddComplaint: TEdit;
    btnSave: TButton;
    lblChooseStudent: TLabel;
    lblCompList: TLabel;
    lblEditComp: TLabel;
    btnRemove: TButton;
    lblCurrentWeek: TLabel;
    btnPrevWeek: TButton;
    btnNextWeek: TButton;
    btnCurrentWeek: TButton;
    lvComplaints: TListView;
    procedure LoadStudentstoCB(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btnPrevWeekClick(Sender: TObject);
    procedure InitializeDay(Sender: TObject; dayOfMonday: TDateTime);
    procedure btnNextWeekClick(Sender: TObject);
    procedure btnCurrentWeekClick(Sender: TObject);
    procedure loadUserComplaintsToLb(Sender: TObject; UserName: string);
    procedure btnSaveClick(Sender: TObject);
    procedure cbbStudentsChange(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure lvComplaintsClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmUserComplaints: TfrmUserComplaints;
  ComplaintsList: PComplaints;

implementation
uses
  System.DateUtils, MainShedule, Marks, Auth;

{$R *.dfm}
var
  CurrentUser: string;
  DateOfMonday: TDateTime;

procedure DisposeEl(LastMark: PComplaints);
var
  temp : PComplaints;
begin
  temp := LastMark;
  if LastMark^.NextComp <> nil then
    LastMark^.NextComp := LastMark^.NextComp.NextComp;
  Dispose(temp);
end;

procedure DisposeMarksStruct(var PList: PComplaints);
var
  P: PComplaints;
  Temp: PComplaints;
begin
  P := PList;
  while Assigned(P) do
  begin
    Temp := P;
    P := P^.NextComp;
    Dispose(Temp);
  end;
end;
procedure AddComp(const CompDate: TDateTime; UserName: String; Value: Shortstring;
                var ComplaintsList: PComplaints);
var
  P: PComplaints;
begin
  New(P);
  P^.Complaint.Date := CompDate;
  P^.Complaint.Username := UserName;
  P^.Complaint.Value := Value;
  P^.NextComp := ComplaintsList;
  ComplaintsList := P;
end;

procedure DeleteComp(var pHead: PComplaints; const Edit1: TEdit);
var
  P: PComplaints;
begin
  P := pHead;
  if P^.Complaint.Value = ShortString(Edit1.Text) then
    pHead := P^.NextComp;
  while Assigned(P) and Assigned(P^.NextComp) do
  begin
      if (P^.NextComp^.Complaint.Value = ShortString(Edit1.Text)) and
       (P^.nextComp^.Complaint.Username = CurrentUser) then
      begin
        P^.NextComp := P^.NextComp^.NextComp;
      end;
    P := P^.NextComp;
  end;
end;

procedure LoadUserComplaintsFromFile(const UserName: string; var ComplaintsList: PComplaints);
var
  CompFile: File of TComplaint;
  NewComp: TComplaint;
  P: PComplaints;
begin
  P := ComplaintsList;

  if FileExists('Complaints.dat') then
  begin
    AssignFile(CompFile, 'Complaints.dat');
    Reset(CompFile);
    while not EoF(CompFile) do
    begin
      read(CompFile, NewComp);
      AddComp(NewComp.Date, NewComp.Username, NewComp.Value, ComplaintsList);
    end;
    CloseFile(CompFile);
  end;
end;

procedure SaveUserComplaintsToFile(const UserName: string; var ComplaintsList: PComplaints);
var
  CompFile: File of TComplaint;
  P: PComplaints;
begin
  P := ComplaintsList;
  AssignFile(CompFile, 'Complaints.dat');
  Rewrite(CompFile);
  while Assigned(P) do
  begin
    Write(CompFile, P^.Complaint);
    P := P^.NextComp;
  end;
  CloseFile(CompFile);
end;

procedure TfrmUserComplaints.loadUserComplaintsToLb(Sender: TObject; UserName: string);
var
  P: PComplaints;
begin
  P := ComplaintsList;
  lvComplaints.Clear;
  while Assigned(P) do
  begin
    if (P^.Complaint.Username = UserName) and (P^.Complaint.Date = CurrentMonday)then
      lvComplaints.AddItem(P^.Complaint.Value, Sender);
    P := P^.NextComp;
  end;
end;

procedure TfrmUserComplaints.lvComplaintsClick(Sender: TObject);
begin
  if lvComplaints.ItemIndex <> -1 then
    edtAddComplaint.Text := lvComplaints.Items[lvComplaints.ItemIndex].Caption;
end;

procedure TfrmUserComplaints.btnCurrentWeekClick(Sender: TObject);
begin
  CurrentMonday := getDateOfMonday(Date);
  InitializeDay(Sender, CurrentMonday);
end;

procedure TfrmUserComplaints.btnNextWeekClick(Sender: TObject);
begin
  CurrentMonday := IncDay(CurrentMonday, 7);
  InitializeDay(Sender, CurrentMonday);
end;

procedure TfrmUserComplaints.btnPrevWeekClick(Sender: TObject);
begin
  CurrentMonday := IncDay(CurrentMonday, -7);
  InitializeDay(Sender, CurrentMonday);
end;

procedure TfrmUserComplaints.btnRemoveClick(Sender: TObject);
begin
  DeleteComp(ComplaintsList, edtAddComplaint);
  lvComplaints.Clear;
  loadUserComplaintsToLb(Sender, CurrentUser);
  edtAddComplaint.Clear;
  SaveUserComplaintsToFile(CurrentUser, ComplaintsList);
end;

procedure TfrmUserComplaints.btnSaveClick(Sender: TObject);
begin
  if (edtAddComplaint.Text <> '') and (CurrentUser <> '') then
  begin
    AddComp(CurrentMonday, CurrentUser, edtAddComplaint.Text, ComplaintsList);
    lvComplaints.AddItem(edtAddComplaint.Text, Sender);
    SaveUserComplaintsToFile(CurrentUser, ComplaintsList);
    ShowMessage('Успешно сохранено!');
  end
  else
    ShowMessage('Введены некорректные данные');
end;

procedure TfrmUserComplaints.cbbStudentsChange(Sender: TObject);
begin
  CurrentUser := getLoginByID(cbbStudents.ItemIndex, StudentsList);
  LoadUserComplaintsFromFile(CurrentUser, ComplaintsList);
  loadUserComplaintsToLb(Sender, CurrentUser);
end;

Procedure TfrmUserComplaints.InitializeDay(Sender: TObject; dayOfMonday: TDateTime);
begin
    lblCurrentWeek.Caption := DateToStr(dayOfMonday) + '-' + DateToStr(incDay(dayOfMonday, 6));
    if CurrentUser <> '' then
    begin
      loadUserComplaintsToLb(Sender, CurrentUser);
    end;
end;

procedure TfrmUserComplaints.FormActivate(Sender: TObject);
begin
  cbbStudents.Clear;
  LoadStudentstoCB(Sender);
  CurrentUser := '';
  CurrentMonday := getDateOfMonday(Date);
  InitializeDay(Sender, CurrentMonday);
end;

procedure TfrmUserComplaints.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  SaveUserComplaintsToFile(CurrentUser, ComplaintsList);
end;

procedure TfrmUserComplaints.LoadStudentstoCB(Sender: TObject);
var
  P: PPwList;
begin
  if not Assigned(StudentsList) then
    getStudentsList(StudentsList);
  P := StudentsList;
  while Assigned(P) do
  begin
    cbbStudents.AddItem(P^.Data.UserName, Sender);
    P := P^.mNext;
  end;
end;
end.
