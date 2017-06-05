open GtkSignal
open Gobject
open Data
let set = set
let get = get
let param = param
open GtkAssistantProps

class virtual assistant_sigs = object (self)
  method private virtual connect :
    'b. ('a,'b) GtkSignal.t -> callback:'b -> GtkSignal.id
  method apply = self#connect Assistant.S.apply
  method cancel = self#connect Assistant.S.cancel
  method close = self#connect Assistant.S.close
  method leave = self#connect Assistant.S.leave
  method prepare = self#connect Assistant.S.prepare
end

