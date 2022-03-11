### ROTINA DE IMPORTAÇÃO E TRATAMENTO DE DADOS P/ BOLETIM DE ECON. INTERNACIONAL - GECE ###
## 1º Passo: Instalação e carregamento de pacotes

if (!require("pacman")) install.packages("pacman")
pacman::p_load("tidyverse","rbcb","sidrar","timetk","purrr","tibble")

## 2º Passo: Coleta de dados
# Estrutura dos comandos
Variável <- get_series(c(Variável='Código da Variável'), last = 'Recorte de tempo', as = "Tipo do objeto")
View(Variável)
ggplot(data = Variável) + 
  geom_line(aes(x = date, y = Variável, colour = Variável))

# Conta Capital Trimestral

Conta_Capital <- get_series(c(Conta_Capital = 23231), last = 5, as = "tibble")

# Conta Financeira
  # Investimento Direto Trimestral

IDE <- get_series(c(IDE=23245), last = 5, as = "tibble")

IDP <- get_series(c(IDP=23265), last = 5, as = "tibble")

  # Investimentos em Carteira Trimestral

Invest_Cart <- get_series(c(Variável=23285), last = 5, as = "tibble")

  # Derivativos trimestral

Derivativos <- get_series(c(Derivativos=23346), last = 5, as = "tibble")

  # Outros Investimentos trimestral

Outros_Invest <- get_series(c(Outros_Invest=23349), last = 5, as = "tibble")

  # Ativos de Reserva trimestral

Ativos_de_Reserva <- get_series(c(Ativos_de_Reserva=23423), last = 5, as = "tibble")
  
  # Investimento Direto ao país mensal

IDPm = get_series(c(IDPm=22885), last = 13, as = "tibble")

  # PIB mensal corrente (US$)

PIBm = get_series(c(PIBm=4385), last = 13, as = "tibble")

  # PIB acumulado dos últimos 12 meses (US$)

PIBac = get_series(c(PIBac=4192), last = 13, as = "tibble")

## 3º Passo: Tratamento dos dados
# Tratando e juntando os DataFrames da CF
Conta_Capital <- Conta_Capital %>% 
  filter(Conta_Capital<130)

Invest_Direto = inner_join(IDE,IDP)
Invest_Dir_Cart = inner_join(Invest_Direto,Invest_Cart)
Invest_Dir_Cart_Deriv = inner_join(Invest_Dir_Cart,Derivativos)
Ativ_Invest_Dir_Cart_Deriv = inner_join(Ativos_de_Reserva,Invest_Dir_Cart_Deriv)
Ativ_Invest_Dir_Cart_Deriv_Outr = inner_join(Ativ_Invest_Dir_Cart_Deriv,Outros_Invest)
Conta_Financeira = inner_join(Invest_Dir_Cart_Deriv,Ativ_Invest_Dir_Cart_Deriv_Outr)
CCF = full_join(Conta_Financeira,Conta_Capital)
foo<-function(v) {
  ifelse(is.na(v),
         mean(v,na.rm = TRUE),
         v)
}
CCF = CCF %>% 
  mutate(Conta_Capital=foo(Conta_Capital))
CCF = CCF %>% 
  select(
    Trimestre = 'date',
    IDE,
    IDP,
    Investimento_em_Carteira = 'Variável',
    Derivativos,
    Ativos_de_Reserva,
    Outros_Investimentos = 'Outros_Invest',
    Conta_Capital
  )
View(CCF)
IDP_PIB = full_join(IDPm,PIBm)
# Cálculo da acumulação nos últimos 4 trimestres (12 meses)
  # CC
Acumul_CC = CCF %>% 
  mutate(Acumul_CC = sum(CCF$Conta_Capital)-CCF$Conta_Capital[1]) %>% 
  select(Trimestre,Acumul_CC)
Acumul_CC = slice(Acumul_CC,5)
  # IDE e IDP
Acumul_IDE = CCF %>% 
  mutate(Acumul_IDE = sum(CCF$IDE)-CCF$IDE[1]) %>% 
  select(Trimestre,Acumul_IDE)
Acumul_IDE=slice(Acumul_IDE,5)
Acumul1 = full_join(Acumul_CC,Acumul_IDE)
Acumul_IDP = CCF %>% 
  mutate(Acumul_IDP = sum(CCF$IDP)-CCF$IDP[1]) %>% 
  select(Trimestre,Acumul_IDP)
Acumul_IDP=slice(Acumul_IDP,5)
Acumul2 = full_join(Acumul1,Acumul_IDP)
  # Investimento em Carteira
Acumul_Invest_Cart = CCF %>% 
  mutate(Acumul_Invest_Cart = sum(CCF$Investimento_em_Carteira)-CCF$Investimento_em_Carteira[1]) %>% 
  select(Trimestre,Acumul_Invest_Cart)
Acumul_Invest_Cart=slice(Acumul_Invest_Cart,5)
Acumul3 = full_join(Acumul2,Acumul_Invest_Cart)
  # Derivativos
Acumul_Deriv = CCF %>% 
  mutate(Acumul_Deriv = sum(CCF$Derivativos)-CCF$Derivativos[1]) %>% 
  select(Trimestre,Acumul_Deriv)
Acumul_Deriv=slice(Acumul_Deriv,5)
Acumul4 = full_join(Acumul3, Acumul_Deriv)
  # Ativos de Reserva
Acumul_Ativos_Reserv = CCF %>% 
  mutate(Acumul_Ativos_Reserv = sum(CCF$Ativos_de_Reserva)-CCF$Ativos_de_Reserva[1]) %>% 
  select(Trimestre,Acumul_Ativos_Reserv)
Acumul_Ativos_Reserv=slice(Acumul_Ativos_Reserv,5)
Acumul5 = full_join(Acumul4,Acumul_Ativos_Reserv)
  # Outros Investimentos
Acumul_Outr_Invest = CCF %>% 
  mutate(Acumul_Outr_Invest = sum(CCF$Outros_Investimentos)-CCF$Outros_Investimentos[1]) %>% 
  select(Trimestre,Acumul_Outr_Invest)
Acumul_Outr_Invest=slice(Acumul_Outr_Invest,5)
Acumulacao = full_join(Acumul5,Acumul_Outr_Invest)
View(Acumulacao)
# Cálculo da variação marginal (com relação ao trimestre imediatamente anterior)
# CC
Var_marg_CC = CCF %>% 
  mutate(Var_marg_CC = (((CCF$Conta_Capital)/lag(CCF$Conta_Capital)-1)*100)) %>% 
  select(Trimestre,Var_marg_CC)
# IDE e IDP
Var_marg_IDE=CCF %>% 
  mutate(Var_marg_IDE = (((CCF$IDE)/lag(CCF$IDE)-1)*100)) %>% 
  select(Trimestre,Var_marg_IDE)
Varmar1 = full_join(Var_marg_CC,Var_marg_IDE)
Var_marg_IDP=CCF %>% 
  mutate(Var_marg_IDP = (((CCF$IDP)/lag(CCF$IDP)-1)*100)) %>% 
  select(Trimestre,Var_marg_IDP)
Varmar2 = full_join(Varmar1,Var_marg_IDP)
# Investimento em Carteira
Var_marg_Invest_Cart=CCF %>% 
  mutate(Var_marg_Invest_Cart = (((CCF$Investimento_em_Carteira)/lag(CCF$Investimento_em_Carteira)-1)*100)) %>% 
  select(Trimestre,Var_marg_Invest_Cart)
Varmar3 = full_join(Varmar2, Var_marg_Invest_Cart)
# Derivativos
Var_marg_Deriv=CCF %>% 
  mutate(Var_marg_Deriv = (((CCF$Derivativos)/lag(CCF$Derivativos)-1)*100)) %>% 
  select(Trimestre,Var_marg_Deriv)
Varmar4 = full_join(Varmar3,Var_marg_Deriv)
# Ativos de Reserva
Var_marg_Ativ_Reserv=CCF %>% 
  mutate(Var_marg_Ativ_Reserv = (((CCF$Ativos_de_Reserva)/lag(CCF$Ativos_de_Reserva)-1)*100)) %>% 
  select(Trimestre,Var_marg_Ativ_Reserv)
Varmar5 = full_join(Varmar4,Var_marg_Ativ_Reserv)
# Outros Investimentos
Var_marg_Outr_Invest=CCF %>% 
  mutate(Var_marg_Outr_Invest = (((CCF$Outros_Investimentos)/lag(CCF$Outros_Investimentos)-1)*100)) %>% 
  select(Trimestre,Var_marg_Outr_Invest)
Variacao_marginal = full_join(Varmar5, Var_marg_Outr_Invest)
View(Variacao_marginal)
# Cálculo da variação interanual (com relação ao mesmo trimestre do ano anterior)
  # CC
Var_interanual_CC = CCF %>% 
  mutate(Var_interanual_CC = (((CCF$Conta_Capital)/lag(CCF$Conta_Capital,4)-1)*100)) %>%
  select(Trimestre,Var_interanual_CC)
  # IDE e IDP
Var_interanual_IDE = CCF %>% 
  mutate(Var_interanual_IDE = (((CCF$IDE)/lag(CCF$IDE,4)-1)*100)) %>% 
  select(Trimestre,Var_interanual_IDE)
Varint1 = full_join(Var_interanual_CC,Var_interanual_IDE)
Var_interanual_IDP = CCF %>% 
  mutate(Var_interanual_IDP = (((CCF$IDP)/lag(CCF$IDP,4)-1)*100)) %>% 
  select(Trimestre,Var_interanual_IDP)
Varint2 = full_join(Varint1,Var_interanual_IDP)
  # Investimento em Carteira
Var_interanual_Invest_Cart = CCF %>% 
  mutate(Var_interanual_Invest_Cart = (((CCF$Investimento_em_Carteira)/lag(CCF$Investimento_em_Carteira,4)-1)*100)) %>% 
  select(Trimestre,Var_interanual_Invest_Cart)
Varint3 = full_join(Varint2,Var_interanual_Invest_Cart)
  # Derivativos
Var_interanual_Deriv = CCF %>% 
  mutate(Var_interanual_Deriv = (((CCF$Derivativos)/lag(CCF$Derivativos,4)-1)*100)) %>% 
  select(Trimestre,Var_interanual_Deriv)
Varint4 = full_join(Varint3,Var_interanual_Deriv)
  # Ativos de Reserva
Var_interanual_Ativ_Reserv = CCF %>% 
  mutate(Var_interanual_Ativ_Reserv = (((CCF$Ativos_de_Reserva)/lag(CCF$Ativos_de_Reserva,4)-1)*100)) %>% 
  select(Trimestre,Var_interanual_Ativ_Reserv)
Varint5 = full_join(Varint4,Var_interanual_Ativ_Reserv)
  # Outros Investimentos
Var_interanual_Outr_Invest = CCF %>% 
  mutate(Var_interanual_Outr_Invest = (((CCF$Outros_Investimentos)/lag(CCF$Outros_Investimentos,4)-1)*100)) %>% 
  select(Trimestre,Var_interanual_Outr_Invest)
Variacao_interanual = full_join(Varint5,Var_interanual_Outr_Invest)
View(Variacao_interanual)
# Cálculo do IDPm como % do PIBm
razao_IDP_PIB = IDP_PIB %>% 
  mutate(razao_IDP_PIB = (IDP_PIB$IDPm/IDP_PIB$PIBm)*100) %>% 
  select(
    Mês = "date",
    razao_IDP_PIB
  )
View(razao_IDP_PIB)

razao_IDP_PIB%>% 
  knitr::kable()

ggplot(data = razao_IDP_PIB) + 
  geom_line(aes(x = Mês, y = razao_IDP_PIB, colour = razao_IDP_PIB))
