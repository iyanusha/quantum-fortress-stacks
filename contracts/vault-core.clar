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

;; Deposit STX into vault
(define-public (deposit (vault-id uint) (amount uint))
  (let (
    (owner tx-sender)
    (vault-maybe (map-get? vaults {owner: owner, vault-id: vault-id}))
  )
    ;; Check if vault exists
    (if (is-some vault-maybe)
      (let (
        (vault (unwrap-panic vault-maybe))
        (current-balance (get balance vault))
        (new-balance (+ current-balance amount))
        (now (current-time))
      )
        ;; Transfer STX to contract
        (let ((transfer-result (stx-transfer? amount owner (as-contract tx-sender))))
          (asserts! (is-ok transfer-result) (err ERR-INSUFFICIENT-BALANCE))
        )

        ;; Update vault balance and last accessed time
        (map-set vaults
          {owner: owner, vault-id: vault-id}
          (merge vault {
            balance: new-balance,
            last-accessed: now
          })
        )

        (ok new-balance)
      )
      (err ERR-VAULT-NOT-FOUND)
    )
  )
)

;; Withdraw STX from vault
(define-public (withdraw (vault-id uint) (amount uint))
  (let (
    (owner tx-sender)
    (vault-maybe (map-get? vaults {owner: owner, vault-id: vault-id}))
  )
    ;; Check if vault exists
    (if (is-some vault-maybe)
      (let (
        (vault (unwrap-panic vault-maybe))
        (current-balance (get balance vault))
        (new-balance (- current-balance amount))
        (now (current-time))
      )
        ;; Check if user is vault owner
        (asserts! (is-vault-owner owner vault-id) (err ERR-NOT-AUTHORIZED))

        ;; Check if vault has sufficient balance
        (asserts! (>= current-balance amount) (err ERR-INSUFFICIENT-BALANCE))

        ;; Transfer STX from contract to user
        (let ((transfer-result (as-contract (stx-transfer? amount tx-sender owner))))
          (asserts! (is-ok transfer-result) (err ERR-INSUFFICIENT-BALANCE))
        )

        ;; Update vault balance and last accessed time
        (map-set vaults
          {owner: owner, vault-id: vault-id}
          (merge vault {
            balance: new-balance,
            last-accessed: now
          })
        )

        (ok new-balance)
      )
      (err ERR-VAULT-NOT-FOUND)
    )
  )
)

;; Read-only Functions

;; Get vault details
(define-read-only (get-vault-info (owner principal) (vault-id uint))
  (map-get? vaults {owner: owner, vault-id: vault-id})
)

;; Get total number of vaults
(define-read-only (get-total-vaults)
  (ok (var-get total-vaults))
)
