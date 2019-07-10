unit AuthorizationUnitS;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons;

type
  TAccess = (aTeacher, aParent, aStudent);
  TpwData = packed record
    Password: String[20];
    LogIn: String[20];
    Access: TAccess;
  end;
  PPwList = ^TpwList;
  TPwList = packed record
    Data: TpwData;
    mNext: PPwList;
  end;

  TfrmAuthorization = class(TForm)
    edLogin: TEdit;
    lbLogin: TLabel;
    lbPassword: TLabel;
    edPassword: TEdit;
    btnEnter: TBitBtn;
    btnExit: TBitBtn;
    lbWrong: TLabel;
    procedure FormActivate(Sender: TObject);
    procedure btnEnterClick(Sender: TObject);
    procedure edLoginClick(Sender: TObject);
    procedure btnExitClick(Sender: TObject);
  private
    procedure pwListInitializtion(var PasswordList: PPwList);
    function CheckLogIn(var PasswordList: PpwList; Edit: TEdit): Boolean;
  public
    procedure SavePasswordToFile(var PasswordList: PpwList; const FileName: String);
  end;

var
  frmAuthorization: TfrmAuthorization;
  PasswordList: PPwList;
  AccessRight: TAccess;

implementation

uses
  UsersUnit;
{$R *.dfm}

procedure TfrmAuthorization.btnEnterClick(Sender: TObject);
begin
  if CheckLogIn(PasswordList,edLogin) then
  begin
    Close;
    frmAuthorization.ModalResult := mrOk;
    if AnsiUpperCase(edLogin.Text) = AnsiUpperCase(MainUser) then
      AccessRight := aTeacher
  end
  else
    lbWrong.Caption := 'Неверный логин/пароль'
end;

procedure TfrmAuthorization.btnExitClick(Sender: TObject);
begin
  Close;
end;

function TfrmAuthorization.CheckLogIn(var PasswordList: PpwList; Edit: TEdit): Boolean;
var
  P: PpwList;
begin
  Result := False;
  P := PasswordList;
  while Assigned(P) do
  begin
    if (AnsiUpperCase(String(P^.Data.LogIn)) = (AnsiUpperCase(Edit.Text))) and
      (P^.Data.Password = ShortString(Edit.Text)) then
      Result := True;
    P := P^.mNext;
  end;
end;

procedure TfrmAuthorization.edLoginClick(Sender: TObject);
begin
  lbWrong.Caption := '';
end;

procedure TfrmAuthorization.FormActivate(Sender: TObject);
begin
  lbWrong.Caption := '';
  PasswordList := nil;
  pwListInitializtion(PasswordList);
end;

procedure TfrmAuthorization.pwListInitializtion(var PasswordList: PPwList);
var
  StandartUser: TpwData;
  P: PPwList;
  F: File of TpwData;
begin
  AssignFile(F,'users.pw');
  if (FileExists('users.pw')) then
    Reset(F)
  else
  begin
    Rewrite(F);
    StandartUser.LogIn := 'teacher';
    StandartUser.Password := '';
    write(F,StandartUser);
    CloseFile(F);
    Reset(F);
  end;

  while not (EOF(F)) do
  begin
    New(P);
    Read(F,P^.Data);
    P^.mNext := PasswordList;
    PasswordList := P;
  end;
  CloseFile(F);
end;

procedure TfrmAuthorization.SavePasswordToFile(var PasswordList: PpwList;
  const FileName: String);
var
  F: File of TpwData;
  P: PpwList;
begin
  AssignFile(F,FileName);
  Rewrite(F);
  P := PasswordList;
  while Assigned(P) do
  begin
    write(F, P^.Data);
    P := P^.mNext
  end;
  CloseFile(F);
end;

end.
