package Chess_Board is
	
	type T_Team is (None, Black, White);
	type T_Family is (None, Pawn, Rook, Bishop, Knight, Queen, King);
	type T_Cases is array(Natural range 0..63) of Natural;
	type T_Piece is record
		Family : T_Family;
		Team : T_Team;
	end record;
	type T_Chessboard is array(Natural range 0..63) of T_Piece;
	
	Empty_Piece : T_Piece := (Family => None, Team => None);

	-- Retourn le symbole coloré de la pièce
	function Symbol(Piece : in T_Piece) return String;

	-- Enum to String
	function Translate_Team(Team : in T_Team) return String;

	-- Remettre à zéro la position de l'échequier
	procedure Init_Board(Board : out T_Chessboard);

	-- Afficher l'échequier
	procedure Display_Board(Board : in T_Chessboard ; Reversed : in Boolean := False ; Highlight : in T_Cases := (others => 0));

	-- Convertir une notation echec en case
	function Get_Case_Id(X : in String) return Natural;

	-- Le move From => Dest est-il legal ?
	function Is_Legal_Move(Board : in T_Chessboard ; Turn : in T_Team ; From, Dest : in Natural ; CanEatKing : in Boolean := False) return Boolean;

	-- Obtenir les coups légaux pour une pièce
	function Legal_Moves(Board : in T_Chessboard ; Turn : in T_Team ; Pos : in Natural) return T_Cases;

	-- Obtenir le nombre de coups légaux dispo pour un joueur
	function Legal_Moves_Count(Board : in T_Chessboard ; Turn : in T_Team) return Natural;

	-- Renvoie vrai ssi le roi précisé est en échec
	function Is_Check(Board : in T_Chessboard ; Team : in T_Team) return Boolean;

	-- Copie le plateau dans un autre plateau
	procedure Copy_Board(X : in T_Chessboard ; Y : out T_Chessboard);

end Chess_Board;