unit Author;

{
  Class: TBook
  Introduce: These class maps a record data in table Authors
  Date: 24/02/2025
}

interface

type
  TAuthor = class
    FirstName: string;
    LastName: string;
    ContactUrl: string;
    ResidentCity: string;
    ResidentCountry: string;
  private
    {Private declaratuons}
  public
    {public declaratuons}
    constructor Create(FirstName: string; LastName: string; ContactUrl: string; City: string; Country: string);
    function GetFullName(): string;
    function GetContactUrl(): string;
    function GetResidentCity(): string;
    function GetResidentCountry(): string;
  end;

implementation

constructor TAuthor.Create(FirstName: string; LastName: string; ContactUrl: string; City: string; Country: string);
begin
  self.FirstName := FirstName;
  self.LastName := LastName;
  self.ContactUrl := ContactUrl;
  self.ResidentCity := City;
  self.ResidentCountry := Country;
end;

function TAuthor.GetFullName(): string;
begin
  Result := FirstName + ' ' + LastName;
end;

function TAuthor.GetContactUrl(): string;
begin
  Result := ContactUrl;
end;

function TAuthor.GetResidentCity(): string;
begin
  Result := ResidentCity;
end;

function TAuthor.GetResidentCountry(): string;
begin
  Result := ResidentCountry;
end;

end.
