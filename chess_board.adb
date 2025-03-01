with Ada.Text_IO;
use Ada.Text_IO;

with Ada.Characters.Latin_1;
use Ada.Characters.Latin_1;

package body Chess_Board is

	Numbers : array(1..8) of String(1..3) := ("①", "②", "③", "④", "⑤", "⑥", "⑦", "⑧");
	Letters : array(1..8) of String(1..4) := ("🅐", "🅑", "🅒", "🅓", "🅔", "🅕", "🅖", "🅗");

	-- Retourn le symbole coloré de la pièce
	function Symbol(Piece : in T_Piece) return String is
		Prefix : String(1..4);
	begin
		if Piece.Team = White then
			Prefix := "[97m";
		else
			Prefix := "[30m";
		end if;

		case Piece.Family is
			when None   => return " ";
			when Pawn   => return ESC & Prefix & "♙ ";
			when Rook   => return ESC & Prefix & "♖ ";
			when Bishop => return ESC & Prefix & "♗ ";
			when Knight => return ESC & Prefix & "♘ ";
			when Queen  => return ESC & Prefix & "♕ ";
			when King   => return ESC & Prefix & "♔ ";
		end case;
	end Symbol;

	-- Enum to String
	function Translate_Team(Team : in T_Team) return String is
	begin
		case Team is
			when White => return "Blancs";
			when Black => return "Noirs";
			when others => return "???";
		end case;
	end Translate_Team;

	-- Remettre à zéro la position de l'échequier
	procedure Init_Board(Board : out T_Chessboard) is
	begin
		-- Créer une array vide
		Board := (others => Empty_Piece);

		-- Première ligne des noirs
		Board(0) := (Rook, Black);
		Board(1) := (Knight, Black);
		Board(2) := (Bishop, Black);
		Board(3) := (Queen, Black);
		Board(4) := (King, Black);
		Board(5) := (Bishop, Black);
		Board(6) := (Knight, Black);
		Board(7) := (Rook, Black);

		-- Première ligne des blancs
		Board(63) := (Rook, White);
		Board(62) := (Knight, White);
		Board(61) := (Bishop, White);
		Board(60) := (King, White);
		Board(59) := (Queen, White);
		Board(58) := (Bishop, White);
		Board(57) := (Knight, White);
		Board(56) := (Rook, White);

		-- Placement des pions
		for I in 0..7 loop
			Board(8+I) := (Pawn, Black);
			Board(55-I) := (Pawn, White);
		end loop;
	end Init_Board;

	-- Afficher l'échequier
	procedure Display_Board(Board : in T_Chessboard ; Reversed : in Boolean := False ; Highlight : in T_Cases := (others => 0)) is
		Col, Line : Natural;
		CurCase : Natural;
		Piece : T_Piece;
		Skip : Boolean;
	begin

		if Reversed then
			Line := 9;
			while Line >= 0 loop
				Col := 9;
				
				while Col >= 0 loop
					Skip := False;

					-- Coin: case rouge
					if (Line = 0 and Col = 0) or (Line = 0 and Col = 9) or (Line = 9 and Col = 0) or (Line = 9 and Col = 9) then
						Put("  ");
					
					-- Première et dernière ligne: lettres
					elsif Line = 0 or Line = 9 then
						Put(ESC & "[42m" & Letters(Col) & " " & ESC & "[0m");

					-- Première et dernière colonne: nombres
					elsif Col = 0 or Col = 9 then
						Put(ESC & "[42m" & Numbers(9-Line) & " " & ESC & "[0m");						

					-- Case de l'échequier
					else
						CurCase := (Line-1)*8 + Col-1;
						Piece := Board(CurCase);

						-- Case rouge si échec
						if Piece.Family = King then
							if Is_Check(Board, Piece.Team) then
								Put(ESC & "[41m" & Symbol(Piece) & ESC & "[0m");
								Skip := True;
							end if;
						end if;

						if not Skip then
							-- Case en surbrillance
							if Highlight(CurCase) /= 0 then
								Put(ESC & "[4" & Character'Val(48 + Highlight(CurCase)) & "m" & (if Piece.Family = None then "  " else Symbol(Piece)) & ESC & "[0m");
							
							-- Case claire
							elsif (Line + Col) mod 2 = 0 then
								Put(ESC & "[43m" & (if Piece.Family = None then "  " else Symbol(Piece)) & ESC & "[0m");
							
							-- Case foncée
							else
								Put(ESC & "[48;5;94m" & (if Piece.Family = None then "  " else Symbol(Piece)) & ESC & "[0m");
							end if;
						end if;
					end if;

					if Col > 0 then
						Col := Col - 1;
					else
						exit;
					end if;
				end loop;
				
				New_Line;

				if Line > 0 then
					Line := Line - 1;
				else
					exit;
				end if;
			end loop;
		else
			Line := 0;
			while Line < 10 loop
				Col := 0;
				
				while Col < 10 loop
					Skip := False;

					-- Coin: case rouge
					if (Line = 0 and Col = 0) or (Line = 0 and Col = 9) or (Line = 9 and Col = 0) or (Line = 9 and Col = 9) then
						Put("  ");
					
					-- Première et dernière ligne: lettres
					elsif Line = 0 or Line = 9 then
						Put(ESC & "[42m" & Letters(Col) & " " & ESC & "[0m");

					-- Première et dernière colonne: nombres
					elsif Col = 0 or Col = 9 then
						Put(ESC & "[42m" & Numbers(9-Line) & " " & ESC & "[0m");						

					-- Case de l'échequier
					else
						CurCase := (Line-1)*8 + Col-1;
						Piece := Board(CurCase);
					
						-- Case rouge si échec
						if Piece.Family = King then
							if Is_Check(Board, Piece.Team) then
								Put(ESC & "[41m" & Symbol(Piece) & ESC & "[0m");
								Skip := True;
							end if;
						end if;

						if not Skip then
							-- Case en surbrillance
							if Highlight(CurCase) /= 0 then
								Put(ESC & "[4" & Character'Val(48 + Highlight(CurCase)) & "m" & (if Piece.Family = None then "  " else Symbol(Piece)) & ESC & "[0m");
							
							-- Case claire
							elsif (Line + Col) mod 2 = 0 then
								Put(ESC & "[43m" & (if Piece.Family = None then "  " else Symbol(Piece)) & ESC & "[0m");
							
							-- Case foncée
							else
								Put(ESC & "[48;5;94m" & (if Piece.Family = None then "  " else Symbol(Piece)) & ESC & "[0m");
							end if;
						end if;
					end if;

					Col := Col + 1;
				end loop;
				
				New_Line;

				Line := Line + 1;
			end loop;
		end if;

	end Display_Board;

	-- Convertir une notation echec en case
	function Get_Case_Id(X : in String) return Natural is
		Res : Natural := 0;
	begin

		-- On traite la lettre (colonne)
		case X(X'First) is
			-- when 'a' => Res := Res + 0;
			when 'b' => Res := Res + 1;
			when 'c' => Res := Res + 2;
			when 'd' => Res := Res + 3;
			when 'e' => Res := Res + 4;
			when 'f' => Res := Res + 5;
			when 'g' => Res := Res + 6;
			when 'h' => Res := Res + 7;
			when others => null;
		end case;

		-- On traite le chiffre (ligne)
		case X(X'First + 1) is
			-- when '8' => Res := Res + 0;
			when '7' => Res := Res + 8*1;
			when '6' => Res := Res + 8*2;
			when '5' => Res := Res + 8*3;
			when '4' => Res := Res + 8*4;
			when '3' => Res := Res + 8*5;
			when '2' => Res := Res + 8*6;
			when '1' => Res := Res + 8*7;
			when others => null;
		end case;

		-- Si on est toujours à 0 mais on a donné une case diff de a8 => invalide
		if Res = 0 and X(X'First..X'First+1) /= "a8" then
			return 64; -- 64 <=> invalide
		end if;

		return Res;
	end Get_Case_Id;


	-- Le move From => Dest est-il legal ?
	-- Note: Pour le cas de manger, rien à rajouter sauf pour les pions qui ne mangent pas de la même manière que lorsqu'ils se déplacent
	function Is_Legal_Move(Board : in T_Chessboard ; Turn : in T_Team ; From, Dest : in Natural ; CanEatKing : in Boolean := False) return Boolean is
		Piece : T_Piece := Board(From);
		Piece2 : T_Piece := Board(Dest);

		ColFrom : Natural := From mod 8;
		ColDest : Natural := Dest mod 8;
		LineFrom : Natural := From/8;
		LineDest : Natural := Dest/8;
	begin
		if From = Dest or Piece.Team /= Turn or Piece.Team = Piece2.Team then
			return False;
		end if;

		-- CanEatKing est toujours False, sauf quand on veux vérifier si une position est un échec
		if not CanEatKing and Piece2.Family = King then
			return False;
		end if;

		case Piece.Family is
			when Pawn =>
				-- Un pion ne peut pas changer de colonne que pour manger un autre pion
				if ColFrom /= ColDest then
					-- Si ya pas de pièce on mange pas !
					if Piece2.Family = None then
						return False;
					end if;

					-- on ne peux se déplacer que d'une colonne, et une ligne
					if abs(ColFrom-ColDest) /= 1 or abs(LineFrom-LineDest) /= 1 then
						return False;
					end if;

					-- On ne peux pas manger en arrière avec un pion
					if Piece.Team = White then
						if LineFrom < LineDest then
							return False;
						end if;
					else
						if LineFrom > LineDest then
							return False;
						end if;
					end if;
				end if;

				-- Un pion ne peux pas aller en arrière
				if Piece.Team = White and Dest > From then
					return False;
				elsif Piece.Team = Black and Dest < From then
					return False;
				end if;

				-- Déplacement de deux cases autorisé ?
				if abs(LineDest-LineFrom) = 2 then
					if Piece.Team = Black and LineFrom /= 1 then
						return False;
					elsif Piece.Team = White and LineFrom /= 6 then
						return False;
					end if;

				-- Déplacement de plus de une case jamais possible
				elsif (abs(From-Dest)/8) > 2 then
					return False;
				end if;

				-- Des pièces barrent-t-elles le chemin ?
				-- Une pièce sur la case juste après notre pion ? (+/- 8 <=> remonter ou redescendre de une colonne)
				if Board(From + 8*(if Piece.Team = White then -1 else 1)).Family /= None then
					return False;
				end if;
				-- Pareil sur la 2e case si on se déplace de deux cases
				if abs(LineFrom-LineDest) = 2 and Board(From + 2*8*(if Piece.Team = White then -1 else 1)).Family /= None then
					return False;
				end if;

			when Rook =>
				-- ni vertical ni latéral
				if ColFrom /= ColDest and LineFrom /= LineDest then
					return False;
				end if;

				-- Check si on ne passe pas au dessus d'une autre pièce ?
				-- Cas 1: Vertical
				if ColFrom = ColDest then
					for I in Integer'Min(LineFrom,LineDest)+1..Integer'Max(LineFrom,LineDest)-1 loop
						if Board(I*8+ColFrom).Family /= None then
							return False;
						end if;
					end loop;

				-- Cas 2: Horizontal
				else
					for I in Integer'Min(ColFrom,ColDest)+1..Integer'Max(ColFrom,ColDest)-1 loop
						if Board(LineFrom+I).Family /= None then
							return False;
						end if;
					end loop;
				end if;
			
			when Bishop =>
				-- Dest est-il dans la même diagonale que From ?
				if abs(ColFrom-ColDest) /= abs(LineFrom-LineDest) then
					return False;
				end if;
				
				-- Check si on ne passe pas au dessus d'une autre pièce ?
				if abs(ColFrom-ColDest) > 1 then
					for I in 1..abs(ColFrom-ColDest)-1 loop
						
						-- From est à gauche de Dest
						if Integer'Min(ColFrom,ColDest) = ColFrom then
							-- From est en bas de Dest
							if Integer'Min(LineFrom,LineDest) = LineFrom then
								if Board(LineFrom*8 + ColFrom + 9*I).Family /= None then -- +8 +1
									return False;
								end if;

							-- From est en haut de Dest
							else
								if Board(LineFrom*8 + ColFrom - 7*I).Family /= None then -- -8 +1
									return False;
								end if;
							end if;

						-- From est à droite de Dest
						else
							-- From est en bas de Dest
							if Integer'Min(LineFrom,LineDest) = LineFrom then
								if Board(LineFrom*8 + ColFrom + 7*I).Family /= None then -- +8 -1
									return False;
								end if;

							-- From est en haut de Dest
							else
								if Board(LineFrom*8 + ColFrom - 9*I).Family /= None then -- -8 -1
									return False;
								end if;
							end if;
						end if;

					end loop;
				end if;

			when Knight =>
				if abs(ColFrom-ColDest) = 2 and abs(LineFrom-LineDest) = 1 then
					null;
				elsif abs(ColFrom-ColDest) = 1 and abs(LineFrom-LineDest) = 2 then
					null;
				else
					return False;
				end if;
				-- Chevauchement autorisé

			when Queen =>
				-- Rook + Bishop
				if ColFrom /= ColDest and LineFrom /= LineDest and abs(ColFrom-ColDest) /= abs(LineFrom-LineDest) then
					return False;
				end if;


				-- Check si on passe au dessus d'une pièce
				-- Vertical/Hozirontal => Comme Rook
				if abs(ColFrom-ColDest) /= abs(LineFrom-LineDest) then
					-- Cas 1: Vertical
					if ColFrom = ColDest then
						for I in Integer'Min(LineFrom,LineDest)+1..Integer'Max(LineFrom,LineDest)-1 loop
							if Board(I*8+ColFrom).Family /= None then
								return False;
							end if;
						end loop;

					-- Cas 2: Horizontal
					else
						for I in Integer'Min(ColFrom,ColDest)+1..Integer'Max(ColFrom,ColDest)-1 loop
							if Board(LineFrom+ColFrom).Family /= None then
								return False;
							end if;
						end loop;
					end if;

				-- Diagonale => Bishop
				else
					if abs(ColFrom-ColDest) > 1 then
						for I in 1..abs(ColFrom-ColDest)-1 loop
							
							-- From est à gauche de Dest
							if Integer'Min(ColFrom,ColDest) = ColFrom then
								-- From est en bas de Dest
								if Integer'Min(LineFrom,LineDest) = LineFrom then
									if Board(LineFrom*8 + ColFrom + 9*I).Family /= None then -- +8 +1
										return False;
									end if;

								-- From est en haut de Dest
								else
									if Board(LineFrom*8 + ColFrom - 7*I).Family /= None then -- -8 +1
										return False;
									end if;
								end if;

							-- From est à droite de Dest
							else
								-- From est en bas de Dest
								if Integer'Min(LineFrom,LineDest) = LineFrom then
									if Board(LineFrom*8 + ColFrom + 7*I).Family /= None then -- +8 -1
										return False;
									end if;

								-- From est en haut de Dest
								else
									if Board(LineFrom*8 + ColFrom - 9*I).Family /= None then -- -8 -1
										return False;
									end if;
								end if;
							end if;

						end loop;
					end if;
				end if;

			when King =>
				-- Vertical ou horizontal
				if ColFrom = ColDest or LineFrom = LineDest then
					if (abs(ColFrom-ColDest) + abs(LineFrom-LineDest)) > 1 then
						return False;
					end if;
				elsif abs(ColFrom-ColDest) = abs(LineFrom-LineDest) then
					if (abs(ColFrom-ColDest) + abs(LineFrom-LineDest)) > 2 then
						return False;
					end if;
				else
					return False;
				end if;
				-- Pas de chevauchement possible puisque déplacement de une case seulement

			when others => return False;
		end case;

		-- On vérifie si le move qu'on est en train de faire ne nous met pas potentiellement en échec
		declare
			Future : T_Chessboard;
		begin
			Copy_Board(Board, Future);

			Future(Dest) := Future(From);
			Future(From) := Empty_Piece;

			if Is_Check(Future, Turn) then
				return False;
			end if;
		end;

		return True;
	end Is_Legal_Move;

	-- Obtenir les coups légaux pour une pièce
	function Legal_Moves(Board : in T_Chessboard ; Turn : in T_Team ; Pos : in Natural) return T_Cases is
		Res : T_Cases := (others => 0); -- 0 = illegal, 1 = legal
	begin
		
		for I in 0..63 loop
			if Is_Legal_Move(Board => Board, Turn => Turn, From => Pos, Dest => I) then
				Res(I) := 1;
			end if;
		end loop;

		return Res;
	end Legal_Moves;

	-- Obtenir le nombre de coups légaux dispo pour un joueur
	function Legal_Moves_Count(Board : in T_Chessboard ; Turn : in T_Team) return Natural is
		Count : Natural := 0;
	begin
		for I in 0..63 loop
			for J in 0..63 loop
				if Is_Legal_Move(Board => Board, Turn => Turn, From => I, Dest => J) then
					Count := Count + 1;
				end if;
			end loop;
		end loop;

		return Count;
	end Legal_Moves_Count;

	-- Renvoie vrai ssi le roi précisé est en échec
	function Is_Check(Board : in T_Chessboard ; Team : in T_Team) return Boolean is
		KingPos : Natural := 64;
		Moves : T_Cases;
		Piece : T_Piece;
	begin

		for I in 0..63 loop
			Piece := Board(I);

			if Piece.Team = Team and Piece.Family = King then
				KingPos := I;
				exit;
			end if;
		end loop;

		-- Pas de roi trouvé ??
		if KingPos > 63 then
			return False;
		end if;

		for I in 0..63 loop
			Piece := Board(I);

			if Piece.Team /= Team and Piece.Team /= None then
				if Is_Legal_Move(Board, (if Team = White then Black else White), From => I, Dest => KingPos, CanEatKing => True) then
					return True;
				end if;
			end if;
		end loop;

		return False;
	end Is_Check;


	-- Copie le plateau dans un autre plateau
	procedure Copy_Board(X : in T_Chessboard ; Y : out T_Chessboard) is
	begin
		-- On vide le plateau Y
		Y := (others => Empty_Piece);

		for I in 0..63 loop
			Y(I) := (X(I).Family, X(I).Team);
		end loop;
	end Copy_Board;

end Chess_Board;