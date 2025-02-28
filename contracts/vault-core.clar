;; QuantumFortress - vault-core.clar
;; Core vault functionality for quantum-resistant asset storage

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-VAULT-EXISTS (err u402))
(define-constant ERR-VAULT-NOT-FOUND (err u403))
(define-constant ERR-INVALID-PARAMS (err u404))
(define-constant ERR-LOCKED (err u405))
(define-constant ERR-TIME-LOCK (err u406))
(define-constant ERR-INSUFFICIENT-BALANCE (err u407))

;; Data Maps
;; Map of vaults by owner and vault ID
(define-map vaults 
  { owner: principal, vault-id: uint } 
  { 
    name: (string-ascii 50),
    balance: uint,
    created-at: uint,
    last-accessed: uint,
    time-lock: uint,
    recovery-addresses: (list 5 principal),
    inheritance-active: bool
  }
)

;; Map for storing post-quantum encrypted data for each vault
(define-map vault-encrypted-data
  { owner: principal, vault-id: uint }
  { 
    data-hash: (buff 32),
    encrypted-metadata: (buff 256),
    encryption-version: uint
  }
)

;; Variables
(define-data-var encryption-version uint u1)
(define-data-var total-vaults uint u0)

;; Private Functions
(define-private (is-contract-owner)
  (is-eq tx-sender CONTRACT-OWNER)
)

(define-private (is-vault-owner (owner principal) (vault-id uint))
  (is-eq tx-sender owner)
)

(define-private (current-time)
  (default-to u0 (get-block-info? time (- block-height u1)))
)

(define-private (check-time-lock (owner principal) (vault-id uint))
  (let (
    (vault-maybe (map-get? vaults {owner: owner, vault-id: vault-id}))
  )
    (if (is-some vault-maybe)
      (let (
        (vault (unwrap-panic vault-maybe))
        (time-lock (get time-lock vault))
        (now (current-time))
      )
        (if (> now time-lock)
          (ok true)
          (err ERR-TIME-LOCK)
        )
      )
      (err ERR-VAULT-NOT-FOUND)
    )
  )
)

;; Create a new vault with quantum-resistant encryption
(define-public (create-vault (name (string-ascii 50)) (time-lock uint) (recovery-addresses (list 5 principal)))
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
        last-accessed: now,
        time-lock: (+ now time-lock),
        recovery-addresses: recovery-addresses,
        inheritance-active: false
      }
    )
    
    ;; Initialize encrypted data storage (would contain the post-quantum encrypted key material)
    (map-set vault-encrypted-data
      {owner: owner, vault-id: new-vault-id}
      {
        data-hash: 0x0000000000000000000000000000000000000000000000000000000000000000,
        encrypted-metadata: 0x0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000,
        encryption-version: (var-get encryption-version)
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
        ;; Check time lock
        (let ((time-lock-result (check-time-lock owner vault-id)))
          (asserts! (is-ok time-lock-result) (err ERR-TIME-LOCK))
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
        
        ;; Check time lock
        (let ((time-lock-result (check-time-lock owner vault-id)))
          (asserts! (is-ok time-lock-result) (err ERR-TIME-LOCK))
        )
        
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

;; Update vault time lock
(define-public (update-time-lock (vault-id uint) (new-time-lock uint))
  (let (
    (owner tx-sender)
    (vault-maybe (map-get? vaults {owner: owner, vault-id: vault-id}))
  )
    ;; Check if vault exists
    (if (is-some vault-maybe)
      (let (
        (vault (unwrap-panic vault-maybe))
        (now (current-time))
      )
        ;; Check if user is vault owner
        (asserts! (is-vault-owner owner vault-id) (err ERR-NOT-AUTHORIZED))

        ;; Update vault time lock and last accessed time
        (map-set vaults
          {owner: owner, vault-id: vault-id}
          (merge vault {
            time-lock: (+ now new-time-lock),
            last-accessed: now
          })
        )

        (ok true)
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
