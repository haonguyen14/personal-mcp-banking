export interface ConnectionStatus {
  connected: boolean
}

export interface BankingEngine {
  getLinkToken(): Promise<string>
  exchangePublicToken(publicToken: string): Promise<void>
  checkConnection(): Promise<ConnectionStatus>
}
