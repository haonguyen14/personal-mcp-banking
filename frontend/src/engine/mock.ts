import type { BankingEngine, ConnectionStatus } from './types'

const delay = (ms: number) => new Promise(resolve => setTimeout(resolve, ms))

export const mockEngine: BankingEngine = {
  async getLinkToken(): Promise<string> {
    await delay(300)
    return 'mock-link-token'
  },

  async exchangePublicToken(_publicToken: string): Promise<void> {
    await delay(300)
  },

  async checkConnection(): Promise<ConnectionStatus> {
    await delay(100)
    return { connected: false }
  },
}
