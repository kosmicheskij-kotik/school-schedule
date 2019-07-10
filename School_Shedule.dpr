program School_Shedule;

uses
  Vcl.Forms,
  HomeWorkEditing in 'HomeWorkEditing.pas' {Form2},
  Vcl.Themes,
  Vcl.Styles,
  MainShedule in 'MainShedule.pas' {Form3},
  UsersUnit in 'UsersUnit.pas' {frmUsers},
  Auth in 'Auth.pas' {Form1},
  Marks in 'Marks.pas' {Form4},
  UserComplaints in 'UserComplaints.pas' {frmUserComplaints};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Windows10');
  Application.CreateForm(TForm3, Form3);
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TfrmUsers, frmUsers);
  Application.CreateForm(TfrmAuthorization, frmAuthorization);
  Application.CreateForm(TForm4, Form4);
  Application.CreateForm(TfrmUserComplaints, frmUserComplaints);
  Application.Run;
end.
