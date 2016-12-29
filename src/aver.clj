(def instructions
  [{:name "adc", :opcode-mask 0xFC00, :opcode 0x2C00, :length 2, :src-mask 0x020F, :dst-mask 0x01F0 :src-type :register, :dst-type :register}
   {:name "add", :opcode-mask 0xFC00, :opcode 0x0C00, :length 2, :src-mask 0x020F, :dst-mask 0x01F0, :src-type :register, :dst-type :register}
   {:name "adiw", :opcode-mask 0xFF00, :opcode 0x9600, :length 2, :src-mask 0x00CF, :dst-mask 0x0030, :src-type :immediate, :dst-type :adiw}
   {:name "and", :opcode-mask 0xFC00, :opcode 0x2000, :length 2, :src-mask 0x020F, :dst-mask 0x01F0, :src-type :register, :dst-type :register}
   {:name "andi", :opcode-mask 0xF000, :opcode 0x7000, :length 2, :src-mask 0x0F0F, :dst-mask 0x00F0, :src-type :immediate, :dst-type :register}
   {:name "asr", :opcode-mask 0xFE0F, :opcode 0x9405, :length 2,  :src-mask 0x01F0, :dst-mask 0x01F0, :src-type :register, :dst-type :register, :single-operand true}
   {:name "bld", :opcode-mask 0xFE08, :opcode 0xF800, :length 2,  :src-mask 0x0007, :dst-mask 0x01F0, :src-type :immediate, :dst-type :register}

   ; Keep these last, as there are more specific mnemonics
   {:name "brbc", :opcode-mask 0xFC00, :opcode 0xF400, :length 2,  :src-mask 0x0007, :dst-mask 0x03F8, :src-type :immediate, :dst-type :immediate}
   {:name "brbs", :opcode-mask 0xFC00, :opcode 0xF000, :length 2,  :src-mask 0x0007, :dst-mask 0x03F8, :src-type :immediate, :dst-type :immediate}
   {:name "bclr", :opcode-mask 0xFF8F, :opcode 0x9488, :length 2,  :src-mask 0x0070, :dst-mask 0x0070, :src-type :immediate, :dst-type :immediate, :single-operand true}
   ])

(def reg-sets
  {:adiw [24 26 28 30]})

(defn match-opcode [insn word]
  (let [insn-opcode-mask (:opcode-mask insn)
        insn-opcode (:opcode insn)]
    (== (bit-and word insn-opcode-mask) insn-opcode)))

(defn form-word [[x y]]
  (bit-or (bit-shift-left x 8) y))

(defn append-bit [n set]
  (if set
    (bit-set
     (bit-shift-left n 1)
     0)
    (bit-shift-left n 1)))

(defn extract-bits [mask n]
  (reduce #(append-bit %1 (bit-test n %2)) 0 (filter #(bit-test mask %) (range 31 -1 -1))))

(defn decode-operand [mask type word]
  (let [rawbits (extract-bits mask word)]
    (cond
      (= type :register) (str "r" rawbits)
      (= type :immediate) rawbits
      true (str "r" (nth (type reg-sets) rawbits)))))

(defn instantiate-insn [insn word]
  {:template insn,
   :src (decode-operand (:src-mask insn) (:src-type insn) word)
   :dst (decode-operand (:dst-mask insn) (:dst-type insn) word)
   :mnemonic (:name insn)})

(defn print-insn [insn]
  (let [src (:src insn)
        dst (:dst insn)
        single-operand (get-in insn [:template :single-operand] false)]
    (println
     (format "%s\t%s",
             (:mnemonic insn)
             (clojure.string/join "," (if single-operand [dst] [dst src]))))))

(defn disassemble-word [input]
  (when input
    (let [word (form-word input)]
      (instantiate-insn (first (filter #(match-opcode % word) instructions)) word))))

(defn disassemble [stream]
  (when (seq stream)
    (let [insn (disassemble-word stream)]
      (when insn
        (print-insn insn)
        (recur (drop (get-in insn [:template :length]) stream))))))

