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
