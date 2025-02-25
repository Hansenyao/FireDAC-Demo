unit Book;

{
  Class: TBook
  Introduce: These class maps a record data in table Books
  Date: 24/02/2025
}

interface

type
  TBook = class
    ISBN: string;
    Title: string;
    YearPublished: Integer;
    GenreId: Integer;
    AuthorId: Integer;
  private
    {Private declaratuons}
  public
    {Public declaratuons}
    constructor Create(ISBN: string; Title: string; YearPublished: Integer; GenreId: Integer; AuthorId: Integer);
    function GetISBN(): string;
    function GetTitle(): string;
    function GetYearPublished(): Integer;
    function GetGenerId(): Integer;
    function GetAuthorId(): Integer;
  end;


implementation

constructor TBook.Create(ISBN: string; Title: string; YearPublished: Integer; GenreId: Integer; AuthorId: Integer);
begin
  self.ISBN := ISBN;
  self.Title := Title;
  self.YearPublished := YearPublished;
  self.GenreId := GenreId;
  self.AuthorId := AuthorId;
end;

function TBook.GetISBN(): string;
begin
  Result := self.ISBN;
end;

function TBook.GetTitle(): string;
begin
  Result := self.Title;
end;

function TBook.GetYearPublished(): Integer;
begin
  Result := self.YearPublished;
end;

function TBook.GetGenerId(): Integer;
begin
  Result := self.GenreId;
end;

function TBook.GetAuthorId(): Integer;
begin
  Result := self.AuthorId;
end;

end.
