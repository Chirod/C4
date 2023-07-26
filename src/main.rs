use std::io;
use std::{fmt};
use rand::thread_rng;
use rand::seq::SliceRandom;

trait TournamentGame: minimax::Game {
	fn new_board() -> Self::S;
	fn print_board(b: &Self::S);
}


struct ConnectFour;

const MAX_COLUMS: usize = 7;
const MAX_ROWS: usize = 6;

#[derive(Clone)]
struct ConnectFourBoard {
	last_move: (usize, usize),
	columns:[[Space; MAX_ROWS]; MAX_COLUMS],
	lengths: [usize; MAX_COLUMS]
}

#[derive(Clone, Copy, Eq, PartialEq)]
enum Space {
	Null,
	Empty,
	Yellow,
	Red,		
}

impl fmt::Display for Space {
    // This trait requires `fmt` with this exact signature.
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
				Space::Yellow => 'Y',
				Space::Red => 'R',
				Space::Empty => ' ',
				Space::Null => '!',
		}).unwrap();
        Ok(())
    }
}

impl ConnectFourBoard {

	const CONNECT_FOUR_OPPERTUNITITIES: [[(i8, i8); 4]; 16] = [
			[(-3, 0), (-2, 0), (-1, 0), ( 0, 0)],
			[(-2, 0), (-1, 0), ( 0, 0), ( 1, 0)],
			[(-1, 0), ( 0, 0), ( 1, 0), ( 2, 0)],
			[( 0, 0), ( 1, 0), ( 2, 0), ( 3, 0)],

			[( 0,-3), ( 0,-2), ( 0,-1), ( 0, 0)],
			[( 0,-2), ( 0,-1), ( 0, 0), ( 0, 1)],
			[( 0,-1), ( 0, 0), ( 0, 1), ( 0, 2)],
			[( 0, 0), ( 0, 1), ( 0, 2), ( 0, 3)],

			[( 3,-3), ( 2,-2), ( 1,-1), ( 0, 0)],
			[( 2,-2), ( 1,-1), ( 0, 0), (-1, 1)],
			[( 1,-1), ( 0, 0), (-1, 1), (-2, 2)],
			[( 0, 0), (-1, 1), (-2, 2), (-3, 3)],

			[(-3,-3), (-2,-2), (-1,-1), ( 0, 0)],
			[(-2,-2), (-1,-1), ( 0, 0), ( 1, 1)],
			[(-1,-1), ( 0, 0), ( 1, 1), ( 2, 2)],
			[( 0, 0), ( 1, 1), ( 2, 2), ( 3, 3)],
		];

	fn create() -> Self {
		let empty: [Space; MAX_ROWS] = [Space::Empty; MAX_ROWS];
		Self {
			last_move: (0,0),
			columns: [empty; MAX_COLUMS],
			lengths: [0; MAX_COLUMS]
		}
	}

	fn print(&self) {
		println!("---------------------------------------------");
		for i_r in 0..MAX_ROWS {
			let i = MAX_ROWS - 1 - i_r;
			for v in 0..MAX_COLUMS {
				print!("{}", self.columns[v][i]);
			}
			print!("\n");
		}
	}

	fn mk_move(&mut self, column: usize) {
		if self.lengths[column] < MAX_ROWS && self.columns[column][self.lengths[column]] == Space::Empty {
			self.columns[column][self.lengths[column]] = match self.columns[self.last_move.0][self.last_move.1] {
				Space::Yellow => Space::Red,
				_ => Space::Yellow
			};
			self.last_move = (column, self.lengths[column]);
			self.lengths[column] += 1;
		}
		else {
			panic!()
		}
	}

	fn add_points(p1: (i8, i8), p2: (i8,i8)) -> (i8, i8) {
		let (x1, y1) = p1;
		let (x2, y2) = p2;
		(x1 + x2, y1 + y2)
	}

	fn index_with_point(&self, p: (i8, i8)) -> Space {
		let (x, y) = p;
		if x < 0 || x >= MAX_COLUMS as i8 {
			Space::Null
		}
		else if y < 0 || y >= MAX_ROWS as i8 {
			Space::Null
		}
		else {
			self.columns[x as usize][y as usize]
		}
	}

	fn is_drawn(&self) -> bool {
		self.lengths.iter().all(|&x| x == MAX_ROWS)
	}

	fn score_option(&self, point1: (i8, i8), point2: (i8, i8), point3: (i8, i8), point4: (i8, i8)) -> i16 {
		let mut empties_found = 0;
		let mut red_found = false;
		let mut yellow_found = false;
		for x in [point1, point2, point3, point4] {
			match self.index_with_point(x) {
				Space::Null => return 0,
				Space::Empty => empties_found += 1,
				Space::Red => red_found = true,
				Space::Yellow => yellow_found = true
			}
		}
		let score = match empties_found {
			0 => 1000,
			1 => 100,
			2 => 10,
			3 => 1,
			4 => 0,
			_ => unreachable!(),
		};
		let last_space = self.columns[self.last_move.0][self.last_move.1];
		let r = match (red_found, yellow_found) {
			(true, true) | (false, false) => 0,
			(true, false) => if last_space == Space::Yellow { score } else { -score },
			(false, true) => if last_space == Space::Yellow { -score } else { score },
		};
		// println!("{} {} {} {} -> {}", self.index_with_point(point1),self.index_with_point(point2),self.index_with_point(point3),self.index_with_point(point4), r);
		r
	}

	//Red positive
	fn score_options(&self) -> i16 {
		let mut score = 0;
		for x_r in 0..MAX_COLUMS {
			for y_r in 0..MAX_ROWS {
				let x = x_r as i8;
				let y = y_r as i8;
				score += self.score_option((x,y),(x,y + 1),(x, y + 2),(x, y + 3));
				score += self.score_option((x,y),(x+1,y),(x+2, y),(x + 3, y));
				score += self.score_option((x,y),(x+1,y + 1),(x+2, y + 2),(x + 3, y + 3));
				score += self.score_option((x,y),(x+1,y - 1),(x+2, y - 2),(x + 3, y - 3));
				println!("x: {x}, y: {y}, score: {score}")
			}
		}
		score
	} 

	fn find_win(&self) -> bool {
		let (x, y) = self.last_move;
		let last_placed = self.columns[x][y];
		if last_placed == Space::Empty {
			return false;
		}
		
		Self::CONNECT_FOUR_OPPERTUNITITIES.iter().any(
			|&test_set| test_set.iter().all(
				|&elem| self.index_with_point(Self::add_points((x as i8, y as i8), elem)) == last_placed
			)
		)
	}
}


impl minimax::Game for ConnectFour {
	type S = ConnectFourBoard;
	type M = usize;

	fn generate_moves(state: &Self::S, moves: &mut Vec<Self::M>) {
		for x in 0..MAX_COLUMS {
			if state.lengths[x] < MAX_ROWS {
				moves.push(x);
			}
		}
	}

	fn get_winner(state: &Self::S) -> Option<minimax::Winner> {
		if state.find_win() {
			Some(minimax::Winner::PlayerJustMoved)
		}
		else if state.is_drawn() {
			Some(minimax::Winner::Draw)
		}
		else {
			None
		}
	}

	fn apply(state: &mut Self::S, m: Self::M) -> Option<Self::S> {
		let mut c = state.clone();
		c.mk_move(m);
		Some(c)
	}
}

impl TournamentGame for ConnectFour {
	fn new_board() -> Self::S {
		ConnectFourBoard::create()
	}
	fn print_board(b: &Self::S) {
		b.print()
	}
}

struct NullEval;
impl minimax::Evaluator for NullEval {
	type G = ConnectFour;
	fn evaluate(&self, _s: &<Self::G as minimax::Game>::S) -> minimax::Evaluation {
		return 0;
	}
}

struct ChainCountEval;
impl minimax::Evaluator for ChainCountEval {
	type G = ConnectFour;
	fn evaluate(&self, s: &<Self::G as minimax::Game>::S) -> minimax::Evaluation {
		let score = s.score_options();
		println!("Evaluated as {}: ", score);
		s.print();
		score
	}
}

enum TournamentResult<G: TournamentGame> {
	Winner(TournamentParticipant<G>, TournamentParticipant<G>),
	Draw(TournamentParticipant<G>, TournamentParticipant<G>),
}

struct TournamentParticipant<G: TournamentGame> {
	name: String,
	strategy: Box<dyn minimax::Strategy<G>>,
	points: u64
}

struct User;

impl User {
	fn new() -> Self {
		Self{}
	}
}
impl minimax::Strategy<ConnectFour> for User {
    fn choose_move(&mut self, state: &<ConnectFour as minimax::Game>::S) -> Option<<ConnectFour as minimax::Game>::M> {
        println!("---------------------");
		println!("-------User Choice---");
		println!("---------------------");
		ConnectFour::print_board(state);
		let mut buffer = String::new();
		io::stdin().read_line(&mut buffer).unwrap();
		loop {
			match buffer.chars().next() {
				Some('1') => return Some(0),
				Some('2') => return Some(1),
				Some('3') => return Some(2),
				Some('4') => return Some(3),
				Some('5') => return Some(4),
				Some('6') => return Some(5),
				Some('7') => return Some(6),
				_ => {}
			}
		}
    }
}

impl<G: TournamentGame> TournamentParticipant<G> {
	fn new(strat: impl minimax::Strategy<G> + 'static, name: impl Into<String>) -> Self {
		Self {
			strategy: Box::new(strat),
			name: name.into(),
			points: 0
		}
	}
}

fn compete_strategies<G: TournamentGame>(mut participant_a: TournamentParticipant<G>, mut participant_b: TournamentParticipant<G>) -> TournamentResult<G> {
	let mut board = G::new_board();
	loop {
		let mv = participant_a.strategy.choose_move(&board).unwrap();
		match G::apply(&mut board, mv) {
			None => {},
			Some(new_board) => board = new_board,
		}
		// println!("{} moved:", participant_a.name);
		G::print_board(&board);
		match G::get_winner(&board) {
			Some(minimax::Winner::PlayerJustMoved) => return TournamentResult::Winner(participant_a, participant_b),
			Some(minimax::Winner::PlayerToMove) => return TournamentResult::Winner(participant_b, participant_a),
			Some(minimax::Winner::Draw) => return TournamentResult::Draw(participant_b, participant_a),
			None => {},
		};
		let mv = participant_b.strategy.choose_move(&board).unwrap();
		match G::apply(&mut board, mv) {
			None => {},
			Some(new_board) => board = new_board,
		}
		// println!("{} moved", participant_b.name);
		G::print_board(&board);
		match G::get_winner(&board) {
			Some(minimax::Winner::PlayerJustMoved) => return TournamentResult::Winner(participant_b, participant_a),
			Some(minimax::Winner::PlayerToMove) => return TournamentResult::Winner(participant_a, participant_b),
			Some(minimax::Winner::Draw) => return TournamentResult::Draw(participant_a, participant_b),
			None => {},
		};
	}
}

fn tournament_round<G: TournamentGame>(mut participants: Vec<TournamentParticipant<G>>) -> (Vec<TournamentParticipant<G>>, Vec<TournamentParticipant<G>>, Vec<TournamentParticipant<G>>)  {
	participants.shuffle(&mut thread_rng());
	let mut draws: Vec<TournamentParticipant<G>>  = vec![];
	let mut winners:  Vec<TournamentParticipant<G>> = vec![];
	let mut losers: Vec<TournamentParticipant<G>> = vec![];
	while !participants.is_empty() {
		match (participants.pop(),participants.pop()) {
			(Some(a), Some(b)) => {
				match compete_strategies(a, b) {
					TournamentResult::Winner(the_winner, the_loser) => {winners.push(the_winner); losers.push(the_loser) },
					TournamentResult::Draw(participant_a, participant_b) => {draws.push(participant_a); draws.push(participant_b)},
				}
			}
			(Some(a), None) => {
				draws.push(a);
			}
			(None, _) => unreachable!()
		}
	}
	(winners, draws, losers)
}

fn tournament<G: TournamentGame>(mut participants: Vec<TournamentParticipant<G>>) -> (TournamentParticipant<G>, Vec<TournamentParticipant<G>>) {
	let mut cut = 0;
	let mut cut_participants = vec![];
	while participants.len() > 1 {
		let (mut winners, draws, losers) = tournament_round(participants);
		for a in winners.iter_mut() {
			a.points += 3;
		}
		participants = winners;
		for mut a in draws.into_iter() {
			a.points += 1;
			if a.points >= cut {
				participants.push(a);
			}
			else {
				println!("Cut participant: {} points: {}", a.name, a.points);
				cut_participants.push(a);
			}
		}
		for a in losers.into_iter() {
			if a.points >= cut {
				participants.push(a);
			}
			else {
				println!("Cut participant: {} points: {}", a.name, a.points);
				cut_participants.push(a);
			}
		}
		cut += 2;
	}
	(participants.pop().unwrap(), cut_participants)
}

fn main() {

	match compete_strategies(TournamentParticipant::new(minimax::Negamax::new(NullEval, 5), "Null-5"), TournamentParticipant::new(minimax::Negamax::new(ChainCountEval, 1), "Potential-5")) {
		TournamentResult::Draw(a, b) => println!("draw"),
		TournamentResult::Winner(a, b) => println!("Winner {}", a.name),		
	}

	// let (winner, losers) = tournament(vec![ 
	// 	TournamentParticipant::new(minimax::Negamax::new(NullEval, 1), "Null-1"),
	// 	TournamentParticipant::new(minimax::Negamax::new(NullEval, 2), "Null-2"),
	// 	TournamentParticipant::new(minimax::Negamax::new(NullEval, 3), "Null-3"),
	// 	TournamentParticipant::new(minimax::Negamax::new(NullEval, 4), "Null-4"),
	// 	TournamentParticipant::new(minimax::Negamax::new(NullEval, 5), "Null-5"),
	// 	TournamentParticipant::new(minimax::Negamax::new(NullEval, 6), "Null-6"),
	// 	TournamentParticipant::new(minimax::Negamax::new(NullEval, 7), "Null-7"),
	// 	TournamentParticipant::new(minimax::Negamax::new(NullEval, 8), "Null-8"),
	// 	TournamentParticipant::new(minimax::Negamax::new(NullEval, 9), "Null-9"),
	// 	TournamentParticipant::new(minimax::Negamax::new(NullEval, 10), "Null-10"),
	// 	TournamentParticipant::new(minimax::Negamax::new(ChainCountEval, 1), "Potential-1"),
	// 	TournamentParticipant::new(minimax::Negamax::new(ChainCountEval, 1), "Potential-2"),
	// 	TournamentParticipant::new(minimax::Negamax::new(ChainCountEval, 1), "Potential-3"),
	// 	TournamentParticipant::new(minimax::Negamax::new(ChainCountEval, 1), "Potential-4"),
	// 	TournamentParticipant::new(minimax::Negamax::new(ChainCountEval, 1), "Potential-5"),
	// 	TournamentParticipant::new(minimax::Negamax::new(ChainCountEval, 1), "Potential-6"),
	// 	TournamentParticipant::new(minimax::Negamax::new(ChainCountEval, 1), "Potential-7"),
	// 	TournamentParticipant::new(minimax::Negamax::new(ChainCountEval, 1), "Potential-8"),
	// 	TournamentParticipant::new(minimax::Negamax::new(ChainCountEval, 1), "Potential-9"),
	// 	TournamentParticipant::new(minimax::Negamax::new(ChainCountEval, 1), "Potential-10"),
	// 	// TournamentParticipant::new(User::new(), "User"),
	// ]);
	// println!();
	// for a in losers {
	// 	println!("participant: {} points: {}", a.name, a.points);
	// }
	// println!("Winner: {} points: {}", winner.name, winner.points);

}