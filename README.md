# RustVerifier

## Core Syntax

$$
\newcommand\lifetimecontext{\mathcal{L}}
\def\lvalue{\overset{\leftharpoonup}{v}}
\def\boxt{\square \; \tau}
\begin{align*}
    \text{Lifetime, } \kappa ::= & \textbf{static} \mid \alpha \\
    \text{Access Modifier, } \mu ::= & \textbf{mut} \mid \textbf{shr} \\
    \text{L-Value, } \lvalue ::= & x \mid * \lvalue\\
    \text{Value, } v ::= & z \mid true \mid false \mid \ell \\
    \text{Type} \ni \tau ::= & \textbf{bool} \mid \textbf{int} \mid \boxt
        \mid \&^{\mu} \tau \mid \langle \tau \rangle^{\kappa} \mid () \\
    \text{Lifetime Context, } \Psi ::= & (\lifetimecontext, \preceq) \\
    \text{Expression} \ni e ::= &\: v \mid \text{let mut } x = e
        \mid \text{move } \lvalue \mid \text{copy } \lvalue  \\
        & \mid \lvalue = e \mid \text{box } e \mid \& \lvalue \mid \& \text{mut } \lvalue \\
        & \mid \text{endLifetime } x \\
        & \mid e_1 + e_2 \mid e_1 - e_2
        \mid e_1 \leq e_2 \mid e_1 == e_2  \\
        &\mid \{ e_1; e_2; ...; e_n \}
\end{align*}


$$

## Hoare Forward Rules

$$
\newcommand\lifetimecontext{\mathcal{L}}
\def\lvalue{\overset{\leftharpoonup}{v}}
\def\boxt{\square \; \tau}

% int rule
\begin{equation*}
\frac{}{\{ \Psi, \Delta \} \; z \; \{ \textbf{ens}[r]{ \Psi, \Delta \land r:\mathbf{int}}\}} \quad \text{(int)}
\end{equation*}

\newline

% true rule
\begin{equation*}
\frac{}{\{ \Psi, \Delta \} \; true \; \{ \textbf{ens}[r]{ \Psi, \Delta \land r:\mathbf{bool}}\}} \quad \text{(true)}
\end{equation*}

\newline

% false rule
\begin{equation*}
\frac{}{\{ \Psi, \Delta \} \; false \; \{ \textbf{ens}[r]{ \Psi, \Delta \land r:\mathbf{bool}}\}} \quad \text{(false)}
\end{equation*}

\newline

% box rule
\begin{equation*}
\frac{\{ \Psi, \Delta \} \; e \; \{ \textbf{ens}[r_1]{ \Psi, \Delta \land r_1:\tau}\}}
{\{ \Psi, \Delta \} \; \text{box } e \; \{ \textbf{ens}[r_2]{ \Psi, \Delta \land r_2:\boxt }\}} \quad \text{(box)}
\end{equation*}

\newline

% let-create-lifetime rule
\begin{equation*}
\frac{\textit{fresh } \texttt{'a} \quad \{ \Psi, \Delta \} \; e \; \{ \textbf{ens}[r_1]{ \Psi, \Delta \land r_1 : \tau }\} \quad \neg \text{RefType} (\tau) \quad \Psi_1 \equiv (\lifetimecontext \cup \{ \texttt{'a} \}, \preceq)}
{\{ (\lifetimecontext, \preceq), \Delta \} \; \text{let mut } x = e \; \{ \textbf{ens}[r_2]{ \Psi_1, \Delta \land x: \langle \tau \rangle^{\texttt{'a}} \land r_2 : () }\}} \quad \text{(let-create-lifetime)}
\end{equation*}

\newline

% let-propagate-lifetime rule
\begin{equation*}
\frac{\{ \Psi, \Delta \} \; e \; \{ \textbf{ens}[r_1]{ \Psi_1, \Delta_1 }\}}
{\{\Psi, \Delta \} \; \text{let mut } x = e \; \{ \textbf{ens}[r_2]{ \Psi_1, [r_1 := x]\Delta_1 \land r_2 : () }\}} \quad \text{(let-propagate-lifetime)}
\end{equation*}

\newline

% de-reference rule
\begin{equation*}
\frac{\Delta \vdash \lvalue : \langle \boxt \rangle^{\texttt{'a}} \lor \lvalue : \langle \&^{\mu} \tau \rangle^{\texttt{'a}}}
{\Delta \vdash * \lvalue : \langle \tau \rangle^{\texttt{'a}}} \quad \text{(de-reference)}
\end{equation*}

\newline

% endlifetime rule
\begin{equation*}
\frac{\Delta \vdash x: \langle \tau \rangle^{\texttt{'a}} \quad \texttt{'a} \in \lifetimecontext \quad \Psi_1 \equiv (\lifetimecontext \setminus \{ \texttt{'a} \}, \preceq)}
{\{ (\lifetimecontext, \preceq), \Delta \} \; \text{endLifetime } {x}\; \{ \textbf{ens}[r]{ \Psi_1, \Delta \land r: ()} \}} \quad \text{(endlifetime)}
\end{equation*}

\newline

% move rule
\begin{equation*}
\frac{\textit{fresh } \texttt{'b} \quad \neg \text{readProhibited}(\Psi, \Delta, x, \texttt{'a}) \quad \Psi_1 \equiv ((\lifetimecontext \setminus \{ \texttt{'a} \}) \cup \{ \texttt{'b} \}, \preceq)}
{\{ (\lifetimecontext, \preceq), \Delta \} \; \text{move } x \; \{ \textbf{ens}[r]{ \Psi_1, \Delta \land r: \langle \tau \rangle^{\texttt{'b}} }\}} \quad \text{(move)}
\end{equation*}

\newline

% immutable-borrow rule
\begin{equation*}
\frac{\textit{fresh } \texttt{'b} \quad \Delta \vdash \lvalue: \langle \tau \rangle^{\texttt{'a}} \quad \neg \text{readProhibited}(\Psi, \Delta, \lvalue, \texttt{'a}) \quad \Psi_1 \equiv (\lifetimecontext \cup \{ \texttt{'b} \}, \preceq \cup \{ (\texttt{'a, 'b}) \})}
{\{ (\lifetimecontext, \preceq), \Delta \} \; \& \lvalue \; \{ \textbf{ens}[r]{ \Psi_1, \Delta \land r: \langle \&^{\textbf{shr}} \tau \rangle^{\texttt{'b}}} \}} \quad \text{(shr-borrow)}
\end{equation*}

\newline

% mutable-borrow rule
\begin{equation*}
\frac{\textit{fresh } \texttt{'b} \quad \Delta \vdash \lvalue: \langle \tau \rangle^{\texttt{'a}} \quad \neg \text{writeProhibited}(\Psi, \Delta, \lvalue, \texttt{'a}) \quad \Psi_1 \equiv (\lifetimecontext \cup \{ \texttt{'b} \}, \preceq \cup \{ (\texttt{'a, 'b}) \})}
{\{ (\lifetimecontext, \preceq), \Delta \} \; \&\text{mut } \lvalue \; \{ \textbf{ens}[r]{ \Psi_1, \Delta \land r: \langle \&^{\textbf{mut}} \tau \rangle^{\texttt{'b}}} \}} \quad \text{(mutable-borrow)}
\end{equation*}

\newline

% assignment rule
\begin{equation*}
\frac{\Delta \vdash \lvalue: \langle \tau \rangle^{a} \quad \{ \Psi, \Delta \} \; e \; \{ \textbf{ens}[r_1]{ \Psi, \Delta \land r_1:\tau}\} \quad \neg \text{writeProhibited}(\Psi, \Delta, \lvalue, a)}
{\{ (\lifetimecontext, \preceq), \Delta \} \; \lvalue = e \; \{ \textbf{ens}[r_2]{ \Psi, \Delta \land r_2:() }\}} \quad \text{(assignment)}
\end{equation*}
$$
