;; List of needs from https://www.sociocracyforall.org/nvc-feelings-and-needs-list/

(defhydra dmg-hydra-needs-main  (:color blue :hint nil)
  "
_p_eace     _c_onnection        _a_utonomy  
_m_eaning   _i_nterconnection  c_e_lebration
_h_onesty  c_o_mpetence         _b_asic survival

_q_uit
"
  ("c" dmg-hydra-needs-connection/body)
  ("a" dmg-hydra-needs-autonomy/body)
  ("p" dmg-hydra-needs-peace/body)
  ("i" dmg-hydra-needs-interconnection/body)
  ("m" dmg-hydra-needs-meaning/body)
  ("e" dmg-hydra-needs-celebration/body)
  ("o" dmg-hydra-needs-competence/body)
  ("h" dmg-hydra-needs-honesty/body)
  ("b" dmg-hydra-needs-basicsurvival/body)
  ("q" (org-delete-backward-char 1)))

(defhydra dmg-hydra-needs-connection (:color green :hint nil)
  "
╔═^═════════════════╗
║ ^Connection Needs ║
╚═^═════════════════╝
[_1_] Acceptance
[_2_] Affection
[_3_] Clarity
[_4_] Communication
[_5_] Confirmation
[_6_] Compassion
[_7_] Intimacy
[_8_] Understanding Authenticity
[_9_] Love
[_q_] back
"
  ("1" (dmg-insert-need "Acceptance"))
  ("2" (dmg-insert-need "Affection"))
  ("3" (dmg-insert-need "Clarity"))
  ("4" (dmg-insert-need "Communication"))
  ("5" (dmg-insert-need "Confirmation"))
  ("6" (dmg-insert-need "Compassion"))
  ("7" (dmg-insert-need "Intimacy"))
  ("8" (dmg-insert-need "Understanding Authenticity"))
  ("9" (dmg-insert-need "Love"))
  ("q" dmg-hydra-needs-main/body :exit t))


(defhydra dmg-hydra-needs-autonomy (:color green :hint nil)
  "
╔═^═════════════════╗
║ ^Autonomy Needs   ║
╚═^═════════════════╝
[_1_] Choice
[_2_] Space
[_3_] Spontaneity
[_q_] back
"
  ("q" dmg-hydra-needs-main/body :exit t)
  ("1" (dmg-insert-need "Choice"))
  ("2" (dmg-insert-need "Space"))
  ("3" (dmg-insert-need "Spontaneity")))

(defhydra dmg-hydra-needs-peace (:color green :hint nil)
  "
╔═^═════════════════╗
║ ^Peace Needs      ║
╚═^═════════════════╝
[_1_] Beauty
[_2_] Ease
[_3_] Harmony
[_4_] Order
[_5_] Wholeness
[_q_] back
"
  ("q" dmg-hydra-needs-main/body :exit t)
  ("1" (dmg-insert-need "Beauty"))
  ("2" (dmg-insert-need "Ease"))
  ("3" (dmg-insert-need "Harmony"))
  ("4" (dmg-insert-need "Order"))
  ("5" (dmg-insert-need "Wholeness")))


(defhydra dmg-hydra-needs-interconnection (:color green :hint nil)
  "
╔═^══════════════════════╗
║ ^Interconnection Needs ║
╚═^══════════════════════╝
[_1_] Belonging
[_2_] Consideration
[_3_] Community
[_4_] Cooperation
[_5_] Dignity
[_6_] Mutuality
[_7_] Support
[_8_] Trust
[_q_] back
"
  ("q" dmg-hydra-needs-main/body :exit t)
  ("1" (dmg-insert-need "Belonging"))
  ("2" (dmg-insert-need "Consideration"))
  ("3" (dmg-insert-need "Community"))
  ("4" (dmg-insert-need "Cooperation"))
  ("5" (dmg-insert-need "Dignity"))
  ("6" (dmg-insert-need "Mutuality"))
  ("7" (dmg-insert-need "Support"))
  ("8" (dmg-insert-need "Trust")))


(defhydra dmg-hydra-needs-meaning (:color green :hint nil)
  "
╔═^═══════════════════╗
║ ^Meaning Needs      ║
╚═^═══════════════════╝
[_1_] Contribution
[_2_] Creativity
[_3_] Hope
[_4_] Inspiration
[_5_] Purpose
[_q_] back
"
  ("q" dmg-hydra-needs-main/body :exit t)
  ("1" (dmg-insert-need "Contribution"))
  ("2" (dmg-insert-need "Creativity"))
  ("3" (dmg-insert-need "Hope"))
  ("4" (dmg-insert-need "Inspiration"))
  ("5" (dmg-insert-need "Purpose")))

(defhydra dmg-hydra-needs-celebration (:color green :hint nil)
  "
╔═^════════════════════╗
║ ^Celebration Needs   ║
╚═^════════════════════╝
[_1_] Joy
[_2_] Mourning
[_3_] Play
[_q_] back
"
  ("q" dmg-hydra-needs-main/body :exit t)
  ("1" (dmg-insert-need "Joy"))
  ("2" (dmg-insert-need "Mourning"))
  ("3" (dmg-insert-need "Play")))


(defhydra dmg-hydra-needs-competence (:color green :hint nil)
  "
╔═^══════════════════════╗
║ ^Competence Needs      ║
╚═^══════════════════════╝
[_1_] Effectiveness
[_2_] Efficiency
[_3_] Growth
[_4_] Learning
[_5_] Power
[_q_] back
"
  ("q" dmg-hydra-needs-main/body :exit t)
  ("1" (dmg-insert-need "Effectiveness"))
  ("2" (dmg-insert-need "Efficiency"))
  ("3" (dmg-insert-need "Growth"))
  ("4" (dmg-insert-need "Learning"))
  ("5" (dmg-insert-need "Power")))

(defhydra dmg-hydra-needs-honesty (:color green :hint nil)
  "
╔═^═════════════════╗
║ ^Honesty Needs    ║
╚═^═════════════════╝
[_1_] Authenticity
[_2_] Integrity
[_q_] back
"
  ("q" dmg-hydra-needs-main/body :exit t)
  ("1" (dmg-insert-need "Authenticity"))
  ("2" (dmg-insert-need "Integrity")))

(defhydra dmg-hydra-needs-basicsurvival (:color green :hint nil)
  "
╔═^══════════════════════╗
║ ^Basic Survival Needs  ║
╚═^══════════════════════╝
[_1_] Shelter
[_2_] Food & Water
[_3_] Rest
[_4_] Safety
[_5_] Security
[_6_] Touch
[_q_] back
"
  ("q" dmg-hydra-needs-main/body :exit t)
  ("1" (dmg-insert-need "Shelter"))
  ("2" (dmg-insert-need "Food & Water"))
  ("3" (dmg-insert-need "Rest"))
  ("4" (dmg-insert-need "Safety"))
  ("5" (dmg-insert-need "Security"))
  ("6" (dmg-insert-need "Touch")))
  
(defun dmg-insert-need (need)
  (insert need)
  (newline)
  (insert "- "))
    

(provide 'needs_hydras)
