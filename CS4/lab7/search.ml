(* search.ml: search strategies *)
(* Student name:                *)
(* CMS cluster login name:      *)

module type Storage =
  sig
    type 'a t
    exception Empty

    val create : unit -> 'a t
    val push : 'a -> 'a t -> unit
    val pop : 'a t -> 'a
    val is_empty : 'a t -> bool
  end

module type Domain =
  sig
    type t
    val show : t -> string
    val is_solved : t -> bool
    val compare : t -> t -> int
    val next : t -> t list
  end

module Search (S : Storage) (D : Domain) =
  struct
    module DS = Set.Make(D)

    let search init = 
        let new_s = S.create () in
        let history_init = [init] in
        let vis_boards = DS.empty in
        let check_vis b vis_b = not (DS.mem b vis_b) in
        let rec try_board b vis_b b_his s = 
            if check_vis b vis_b then
                begin
                if D.is_solved b then b_his
                else let children = D.next b in
                List.iter (fun child -> S.push (child::b_his) s) children;
                s_helper (DS.add b vis_b) s;
                end
            else s_helper vis_b s
        and s_helper vis_b s = if S.is_empty s then raise Not_found else
            let pop = S.pop s in
            let curr_b = List.nth pop 0 in
                try_board curr_b vis_b pop s in
        S.push history_init new_s;
        s_helper vis_boards new_s
        

    let show_history hist =
      (String.concat "\n----\n\n" (List.map D.show (List.rev hist))) ^ "\n"
  end

