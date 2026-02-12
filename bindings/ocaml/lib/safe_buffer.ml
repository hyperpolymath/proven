(* SPDX-License-Identifier: Apache-2.0 *)
(* SPDX-FileCopyrightText: 2025 Hyperpolymath *)

(** Safe ring buffer with bounds checking. *)

(** Ring buffer type. *)
type 'a t = {
  mutable data: 'a array;
  mutable head: int;
  mutable tail: int;
  mutable count: int;
  capacity: int;
}

type error =
  | Empty
  | Full
  | Index_out_of_bounds

(** Create a new ring buffer with given capacity. *)
let create capacity default =
  if capacity <= 0 then None
  else Some {
    data = Array.make capacity default;
    head = 0;
    tail = 0;
    count = 0;
    capacity;
  }

(** Get the current number of elements. *)
let length buf = buf.count

(** Get the maximum capacity. *)
let capacity buf = buf.capacity

(** Check if buffer is empty. *)
let is_empty buf = buf.count = 0

(** Check if buffer is full. *)
let is_full buf = buf.count = buf.capacity

(** Available space remaining. *)
let available buf = buf.capacity - buf.count

(** Clear the buffer. *)
let clear buf =
  buf.head <- 0;
  buf.tail <- 0;
  buf.count <- 0

(** Push an element to the back. Returns Error if full. *)
let push_back buf value =
  if is_full buf then Error Full
  else begin
    buf.data.(buf.tail) <- value;
    buf.tail <- (buf.tail + 1) mod buf.capacity;
    buf.count <- buf.count + 1;
    Ok ()
  end

(** Push an element, overwriting oldest if full. *)
let push_back_overwrite buf value =
  if is_full buf then begin
    buf.data.(buf.tail) <- value;
    buf.tail <- (buf.tail + 1) mod buf.capacity;
    buf.head <- (buf.head + 1) mod buf.capacity
  end else begin
    buf.data.(buf.tail) <- value;
    buf.tail <- (buf.tail + 1) mod buf.capacity;
    buf.count <- buf.count + 1
  end

(** Pop an element from the front. *)
let pop_front buf =
  if is_empty buf then Error Empty
  else begin
    let value = buf.data.(buf.head) in
    buf.head <- (buf.head + 1) mod buf.capacity;
    buf.count <- buf.count - 1;
    Ok value
  end

(** Peek at the front element without removing. *)
let peek_front buf =
  if is_empty buf then Error Empty
  else Ok buf.data.(buf.head)

(** Peek at the back element (most recently added). *)
let peek_back buf =
  if is_empty buf then Error Empty
  else
    let idx = (buf.tail - 1 + buf.capacity) mod buf.capacity in
    Ok buf.data.(idx)

(** Get element at index (0 = front). *)
let get buf idx =
  if idx < 0 || idx >= buf.count then Error Index_out_of_bounds
  else
    let real_idx = (buf.head + idx) mod buf.capacity in
    Ok buf.data.(real_idx)

(** Set element at index. *)
let set buf idx value =
  if idx < 0 || idx >= buf.count then Error Index_out_of_bounds
  else begin
    let real_idx = (buf.head + idx) mod buf.capacity in
    buf.data.(real_idx) <- value;
    Ok ()
  end

(** Convert buffer contents to a list (front to back). *)
let to_list buf =
  let rec collect acc i =
    if i < 0 then acc
    else
      let real_idx = (buf.head + i) mod buf.capacity in
      collect (buf.data.(real_idx) :: acc) (i - 1)
  in
  collect [] (buf.count - 1)

(** Convert buffer contents to an array. *)
let to_array buf =
  if buf.count = 0 then [||]
  else begin
    let arr = Array.make buf.count buf.data.(buf.head) in
    for i = 0 to buf.count - 1 do
      let real_idx = (buf.head + i) mod buf.capacity in
      arr.(i) <- buf.data.(real_idx)
    done;
    arr
  end

(** Create buffer from list. *)
let of_list capacity default items =
  match create capacity default with
  | None -> None
  | Some buf ->
      List.iter (fun item -> push_back_overwrite buf item) items;
      Some buf

(** Iterate over elements from front to back. *)
let iter f buf =
  for i = 0 to buf.count - 1 do
    let real_idx = (buf.head + i) mod buf.capacity in
    f buf.data.(real_idx)
  done

(** Iterate with index. *)
let iteri f buf =
  for i = 0 to buf.count - 1 do
    let real_idx = (buf.head + i) mod buf.capacity in
    f i buf.data.(real_idx)
  done

(** Map over elements. *)
let map f buf default =
  match create buf.capacity default with
  | None -> None
  | Some new_buf ->
      iter (fun x ->
        match push_back new_buf (f x) with
        | Ok () -> ()
        | Error _ -> ()
      ) buf;
      Some new_buf

(** Fold from front to back. *)
let fold_left f init buf =
  let acc = ref init in
  for i = 0 to buf.count - 1 do
    let real_idx = (buf.head + i) mod buf.capacity in
    acc := f !acc buf.data.(real_idx)
  done;
  !acc

(** Fold from back to front. *)
let fold_right f buf init =
  let acc = ref init in
  for i = buf.count - 1 downto 0 do
    let real_idx = (buf.head + i) mod buf.capacity in
    acc := f buf.data.(real_idx) !acc
  done;
  !acc

(** Check if any element satisfies predicate. *)
let exists f buf =
  let found = ref false in
  let i = ref 0 in
  while not !found && !i < buf.count do
    let real_idx = (buf.head + !i) mod buf.capacity in
    if f buf.data.(real_idx) then found := true;
    incr i
  done;
  !found

(** Check if all elements satisfy predicate. *)
let for_all f buf =
  let result = ref true in
  let i = ref 0 in
  while !result && !i < buf.count do
    let real_idx = (buf.head + !i) mod buf.capacity in
    if not (f buf.data.(real_idx)) then result := false;
    incr i
  done;
  !result

(** Find first element matching predicate. *)
let find f buf =
  let found = ref None in
  let i = ref 0 in
  while Option.is_none !found && !i < buf.count do
    let real_idx = (buf.head + !i) mod buf.capacity in
    let v = buf.data.(real_idx) in
    if f v then found := Some v;
    incr i
  done;
  !found

(** Filter elements matching predicate. *)
let filter f buf default =
  match create buf.capacity default with
  | None -> None
  | Some new_buf ->
      iter (fun x ->
        if f x then
          match push_back new_buf x with
          | Ok () -> ()
          | Error _ -> ()
      ) buf;
      Some new_buf

(** Drop n elements from front. *)
let drop_front buf n =
  if n <= 0 then ()
  else if n >= buf.count then clear buf
  else begin
    buf.head <- (buf.head + n) mod buf.capacity;
    buf.count <- buf.count - n
  end

(** Take only first n elements. *)
let take_front buf n =
  if n <= 0 then clear buf
  else if n < buf.count then begin
    buf.tail <- (buf.head + n) mod buf.capacity;
    buf.count <- n
  end

(** Resize buffer to new capacity. *)
let resize buf new_capacity default =
  if new_capacity <= 0 then None
  else begin
    let new_data = Array.make new_capacity default in
    let to_copy = min buf.count new_capacity in
    for i = 0 to to_copy - 1 do
      let real_idx = (buf.head + i) mod buf.capacity in
      new_data.(i) <- buf.data.(real_idx)
    done;
    Some {
      data = new_data;
      head = 0;
      tail = to_copy mod new_capacity;
      count = to_copy;
      capacity = new_capacity;
    }
  end

(** Copy the buffer. *)
let copy buf =
  {
    data = Array.copy buf.data;
    head = buf.head;
    tail = buf.tail;
    count = buf.count;
    capacity = buf.capacity;
  }
