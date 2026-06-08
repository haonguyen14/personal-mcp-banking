import type { BankingEngine, ConnectionStatus } from './types'

const BASE_URL = 'http://localhost:8080'

async function post<T>(path: string, body?: unknown): Promise<T> {
  const res = await fetch(`${BASE_URL}${path}`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: body !== undefined ? JSON.stringify(body) : undefined,
  })
  if (!res.ok) throw new Error(`${path} failed: ${res.status}`)
  return res.json()
}

export const apiEngine: BankingEngine = {
  async getLinkToken(): Promise<string> {
    const data = await post<{ linkToken: string }>('/link-token')
    return data.linkToken
  },

  async exchangePublicToken(publicToken: string): Promise<void> {
    const res = await fetch(`${BASE_URL}/exchange-token`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ publicToken }),
    })
    if (!res.ok) throw new Error(`/exchange-token failed: ${res.status}`)
  },

  async checkConnection(): Promise<ConnectionStatus> {
    return post<ConnectionStatus>('/check-connection')
  },
}
