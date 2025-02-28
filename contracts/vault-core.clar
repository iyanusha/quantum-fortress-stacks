;; Vault - A STX storage vault
;; This is the initial implementation that will be enhanced with quantum-resistant features

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-VAULT-EXISTS (err u402))
(define-constant ERR-VAULT-NOT-FOUND (err u403))
(define-constant ERR-INSUFFICIENT-BALANCE (err u404))

;; Data Maps
;; Map of vaults by owner and vault ID
(define-map vaults 
  { owner: principal, vault-id: uint } 
  { 
    name: (string-ascii 50),
    balance: uint,
    created-at: uint,
    last-accessed: uint
  }
)

;; Variables
(define-data-var total-vaults uint u0)

;; Private Functions
(define-private (is-vault-owner (owner principal) (vault-id uint))
  (is-eq tx-sender owner)
)

(define-private (current-time)
  (default-to u0 (get-block-info? time (- block-height u1)))
)

;; Public Functions

;; Create a new vault
(define-public (create-vault (name (string-ascii 50)))
  (let (
    (owner tx-sender)
    (new-vault-id (+ (var-get total-vaults) u1))
    (now (current-time))
    (vault-exists (map-get? vaults {owner: owner, vault-id: new-vault-id}))
  )
    ;; Check if the vault already exists
    (asserts! (is-none vault-exists) (err ERR-VAULT-EXISTS))

    ;; Create new vault
    (map-set vaults
      {owner: owner, vault-id: new-vault-id}
      {
        name: name,
        balance: u0,
        created-at: now,
        last-accessed: now
      }
    )

    ;; Increment total vaults counter
    (var-set total-vaults new-vault-id)

    ;; Return success with new vault ID
    (ok new-vault-id)
  )
)
