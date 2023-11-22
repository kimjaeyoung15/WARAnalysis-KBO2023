# WARfactors-KBO2023
<p>본 프로젝트는 2023년 2학기 한국외국어대학교 국제금융학과 파이낸스어낼리틱스 수업의 프로젝트입니다.</p>
<p>참여 인원: 고우혁, 김재영, 양태승, 이윤우</p>
<h2>1. 프로젝트 개요</h2>
<p>본 프로젝트에서는 WAR(Wins Above Replacement, 대체 수준 대비 승리 기여도)에 영향을 미치는 야구 통계량을 찾고, 이에 대한 함의를 도출하고자 합니다.</p>
<h2>2. 사용 통계 모형과 R 버전</h2>
<p>본 프로젝트에서는 다중선형회귀모형을 사용하며, 이외 모형은 사용하지 않습니다. R은 4.3.2를 사용합니다.<br>
<pre><code>lm()</code></pre>
<h2>3. 사용 패키지</h2>
본 프로젝트에서 사용하는 패키지는 car, ggplot2, GGally, lmtest를 사용합니다.<br>
설치 명령어:<br>
<pre><code>install.packages('car')
install.packages('ggplot2')
install.packages('GGally')
install.packages('lmtest')</code></pre>
<br>
적용 명령어:<br>
<pre><code>library(car)
library(ggplot2)
library(GGally)
library(lmtest)</code></pre>
</p>
<h2>4. 자료의 출처</h2>
<p>본 프로젝트에서 사용하는 자료는 STATIZ의 기록실의 기본 및 확장 통계 자료이며, 선수 50명의 기록을 2023년 전 시즌 자료를 wRC+로 정렬하였습니다.<br>
자료 링크:<br> 
기본 통계 - http://www.statiz.co.kr/stat.php?mid=stat&re=0&ys=2023&ye=2023&se=0&te=&tm=&ty=0&qu=auto&po=0&as=&ae=&hi=&un=&pl=&da=1&o1=WRCPLUS&o2=TPA&de=1&lr=0&tr=&cv=&ml=1&sn=30&si=&cn=<br>
확장 통계 - http://www.statiz.co.kr/stat.php?opt=0&sopt=0&re=0&ys=2023&ye=2023&se=0&te=&tm=&ty=0&qu=auto&po=0&as=&ae=&hi=&un=&pl=&da=2&o1=WRCPLUS&de=1&o2=WAR_ALL&lr=0&tr=&cv=&ml=1&sn=30&si=&cn=</p>
<h2>5. 명명법과 버전 관리</h2>
<p>본 프로젝트에서는 변수, 데이터셋, 모형을 다음과 같이 명명합니다.</p>
<h3>1) 변수</h3>
<p>변수의 이름은 다음 예외를 제외하고 snake 표기법을 사용합니다. 예외 처리된 변수는 폴어 쓰면 길이가 과도하게 길어져 snake 표기법을 변칙 적용하였습니다. 변수 이름에 대한 설명은 variables.txt를 참조 바랍니다.<br>
<pre><code>#snake 표기법의 예시
at_bats</code></pre>
예외: ops, w_oba, w_rcplus, war, spd, hr_ratio, bb_ratio, k_ratio, bbk_ratio, isop, isod, babip, spd, psn, w_rc, w_rc_27o, w_raa, w_oba_pf, w_rc_pf, w_rc_27o_pf, w_raa_pf<br>
<pre><code>#변칙 적용의 예시(o는 아웃을 의미)
wRC/27
w_rc_27o</code></pre>
</p>
<h3>2) 데이터셋</h3>
<p>전체 데이터셋은 Dataset_WARAnalysis_vX.Y로, 전처리된 데이터셋은 Dataset_Cleaned_WARAnalysis_vX.Y로 명명합니다. 여기서 X는 주요한 변화를 나타내며, Y는 부차적인 변화를 나타냅니다. X와 Y는 0 또는 양의 정수입니다.<br>
<pre><code>#데이터셋 명명 예시
Dataset_WARAnalysis_v0.1.csv
Dataset_Cleaned_WARAnalysis_v0.1.csv</code></pre>
</p>
<h3>3) 모형</h3>
<p>전체 데이터셋에 대한 모형은 LM_Prototype_WARAnalysis_vX.Y.Z로, 전처리된 데이터셋에 대한 모형은  LM_Cleared_WARAnalysis_vX.Y.Z로 명명합니다. 여기서 X와 Y는 데이터셋의 버전을, Z는 모형의 버전을 나타냅니다. Z는 1 이상의 자연수입니다. 모형의 버전은 데이터셋의 버전에 종속됩니다.<br>
<pre><code>#모형 명명 예시
LM_Prototype_WARAnalysis_v0.1.1 <- lm()
LM_Cleaned_WARAnalysis_v0.1.1 <- lm()</code></pre>
</p>
